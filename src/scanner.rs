use miette::{Error, LabeledSpan, SourceSpan};

use crate::error::{Eof, SingleTokenError, StringTermError};
use crate::token::{Token, TokenKind};

pub struct Scanner<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
    peeked: Option<Result<Token<'de>, Error>>,
}

/// A scanner for the lox language.
impl<'de> Scanner<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
            peeked: None,
        }
    }

    pub fn expect(&mut self, expected: TokenKind, unexpected: &str) -> Result<Token<'de>, Error> {
        self.expect_where(|next| next.kind == expected, unexpected)
    }

    pub fn expect_where(
        &mut self,
        mut check: impl FnMut(&Token<'de>) -> bool,
        unexpected: &str,
    ) -> Result<Token<'de>, Error> {
        match self.next() {
            Some(Ok(token)) if check(&token) => Ok(token),
            Some(Ok(token)) => Err(miette::miette! {
                labels = vec![LabeledSpan::at(
                    token.offset..token.offset + token.literal.len(), "here"
                )],
                help = format!("Unexpected {token:?}"),
                "{unexpected}",
            }
            .with_source_code(self.whole.to_string())),
            Some(Err(e)) => Err(e),
            None => Err(Eof.into()),
        }
    }

    pub fn peek(&mut self) -> Option<&Result<Token<'de>, Error>> {
        if self.peeked.is_some() {
            return self.peeked.as_ref();
        }

        self.peeked = self.next();
        self.peeked.as_ref()
    }
}

impl<'de> Iterator for Scanner<'de> {
    type Item = Result<Token<'de>, Error>;

    /// Returns the next token after consuming the input.
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(peeked) = self.peeked.take() {
            return Some(peeked);
        }
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let c_at = self.byte;
            let char_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Started {
                Slash,
                IfEqualElse(TokenKind, TokenKind),
                String,
                Number,
                Ident,
            }

            // Single character tokens
            let char_token = |kind: TokenKind| Some(Ok(Token::new(char_str, kind, c_at)));
            let new_token = |kind: TokenKind, literal: &'de str, at: usize| {
                Some(Ok(Token::new(literal, kind, at)))
            };

            let started = match c {
                '(' => return char_token(TokenKind::LeftParen),
                ')' => return char_token(TokenKind::RightParen),
                '{' => return char_token(TokenKind::LeftBrace),
                '}' => return char_token(TokenKind::RightBrace),
                ',' => return char_token(TokenKind::Comma),
                '.' => return char_token(TokenKind::Dot),
                '-' => return char_token(TokenKind::Minus),
                '+' => return char_token(TokenKind::Plus),
                ';' => return char_token(TokenKind::Semicolon),
                '*' => return char_token(TokenKind::Star),
                '/' => Started::Slash,
                '!' => Started::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
                '=' => Started::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '<' => Started::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                '>' => Started::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '"' => Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
                c if c.is_whitespace() => continue,
                _ => {
                    return Some(Err(SingleTokenError {
                        src: self.whole.to_string(),
                        token: c,
                        err_span: SourceSpan::from(self.byte - c.len_utf8()..self.byte),
                    }
                    .into()));
                }
            };

            match started {
                Started::String => {
                    if let Some(end) = self.rest.find('"') {
                        // Found the end of the string
                        let literal = &c_onwards[..end + 1 + 1];
                        self.byte += end + 1;
                        self.rest = &self.rest[end + 1..];
                        return new_token(TokenKind::String, literal, c_at);
                    } else {
                        let e = StringTermError {
                            src: self.whole.to_string(),
                            err_span: SourceSpan::from(self.byte - c.len_utf8()..self.whole.len()),
                        }
                        .into();
                        self.byte += self.rest.len();
                        self.rest = "";
                        return Some(Err(e));
                    }
                }
                Started::Slash => {
                    if self.rest.starts_with('/') {
                        // Single-line comment
                        let end_of_line = self.rest.find('\n').unwrap_or(self.rest.len());
                        let comment = &self.rest[..end_of_line];
                        self.rest = &self.rest[end_of_line..];
                        self.byte += comment.len();
                        // Skip comments
                        continue;
                    } else {
                        return char_token(TokenKind::Slash);
                    }
                }
                Started::IfEqualElse(first, second) => {
                    // Remove whitespace before checking for '='
                    self.rest = self.rest.trim_start();
                    // Characters read from rest including trimmed whitespace excluding current character
                    let trimmed = c_onwards.len() - self.rest.len() - 1;
                    self.byte += trimmed;
                    if self.rest.trim_start().starts_with('=') {
                        // 1 byte is for the '=' character
                        let span = &c_onwards[..c.len_utf8() + trimmed + 1];
                        self.rest = &self.rest[1..];
                        self.byte += 1;
                        return new_token(first, span, c_at);
                    }
                    return char_token(second);
                }
                Started::Number => {
                    let first_non_digit = c_onwards
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or(c_onwards.len());
                    let mut literal = &c_onwards[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');
                    // Check number of occurrences of dots ('.')
                    match (dotted.next(), dotted.next(), dotted.next()) {
                        (Some(one), Some(two), Some(_)) => {
                            // If there are three parts, use only the first two parts, ignore the third
                            literal = &literal[..one.len() + 1 + two.len()];
                        }
                        (Some(one), Some(""), None) => {
                            // If there are two parts, and the second part is empty, use only the first part
                            literal = &literal[..one.len()];
                        }
                        _ => {
                            // do nothing
                        }
                    }
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];
                    let num = match literal.parse() {
                        Ok(num) => num,
                        Err(e) => {
                            return Some(Err(miette::miette!(
                                labels = vec![LabeledSpan::at(
                                    self.byte - literal.len()..self.byte,
                                    "this numeric literal"
                                )],
                                "{e}",
                            )
                            .with_source_code(self.whole.to_string())))
                        }
                    };
                    return new_token(TokenKind::Number(num), literal, c_at);
                }
                Started::Ident => {
                    let first_non_ident = c_onwards
                        .find(|c| !matches!(c, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9'))
                        .unwrap_or(c_onwards.len());
                    let literal = &c_onwards[..first_non_ident];
                    let extra_bytes = literal.len() - c.len_utf8();
                    self.byte += extra_bytes;
                    self.rest = &self.rest[extra_bytes..];
                    let kind = match literal {
                        "and" => TokenKind::And,
                        "class" => TokenKind::Class,
                        "else" => TokenKind::Else,
                        "false" => TokenKind::False,
                        "for" => TokenKind::For,
                        "fun" => TokenKind::Fun,
                        "if" => TokenKind::If,
                        "nil" => TokenKind::Nil,
                        "or" => TokenKind::Or,
                        "return" => TokenKind::Return,
                        "super" => TokenKind::Super,
                        "this" => TokenKind::This,
                        "true" => TokenKind::True,
                        "var" => TokenKind::Var,
                        "while" => TokenKind::While,
                        "print" => TokenKind::Print,
                        _ => TokenKind::Ident,
                    };
                    return new_token(kind, literal, c_at);
                }
            };
        }
    }
}
