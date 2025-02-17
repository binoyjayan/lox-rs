use miette::{Error, LabeledSpan};

use crate::token::{Token, TokenKind};

pub struct Scanner<'de> {
    whole: &'de str,
    rest: &'de str,
    byte: usize,
}

/// A scanner for the lox language.
impl<'de> Scanner<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            rest: input,
            byte: 0,
        }
    }
}

impl<'de> Iterator for Scanner<'de> {
    type Item = Result<Token<'de>, Error>;

    /// Returns the next token after consuming the input.
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let mut chars = self.rest.chars();
            let c = chars.next()?;
            let char_str = &self.rest[..c.len_utf8()];
            let c_onwards = self.rest;
            self.rest = chars.as_str();
            self.byte += c.len_utf8();

            enum Started {
                IfEqualElse(TokenKind, TokenKind),
                String,
                Number,
                Ident,
            }

            // Single character tokens
            let char_token = |kind: TokenKind| {
                Some(Ok(Token {
                    literal: char_str,
                    kind,
                }))
            };
            let new_token = |kind: TokenKind, literal: &'de str| Some(Ok(Token { literal, kind }));

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
                '/' => return char_token(TokenKind::Slash),
                '!' => Started::IfEqualElse(TokenKind::BangEqual, TokenKind::Bang),
                '=' => Started::IfEqualElse(TokenKind::EqualEqual, TokenKind::Equal),
                '<' => Started::IfEqualElse(TokenKind::LessEqual, TokenKind::Less),
                '>' => Started::IfEqualElse(TokenKind::GreaterEqual, TokenKind::Greater),
                '"' => Started::String,
                '0'..='9' => Started::Number,
                'a'..='z' | 'A'..='Z' | '_' => Started::Ident,
                c if c.is_whitespace() => continue,
                _ => {
                    return Some(Err(miette::miette!(
                        labels = vec![LabeledSpan::at(
                            self.byte - c.len_utf8()..self.byte,
                            "this character"
                        )],
                        "Unexpected token '{c}' in input",
                    )
                    .with_source_code(self.whole.to_string())))
                }
            };

            match started {
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
                        return new_token(first, span);
                    }
                    return char_token(second);
                }
                Started::String => todo!(),
                Started::Number => {
                    let first_non_digit = c_onwards
                        .find(|c| !matches!(c, '.' | '0'..='9'))
                        .unwrap_or_else(|| c_onwards.len());
                    let mut literal = &c_onwards[..first_non_digit];
                    let mut dotted = literal.splitn(3, '.');
                    // If there are two dots (i.e. if there are three parts of the split)
                    if let (Some(one), Some(two), Some(_)) =
                        (dotted.next(), dotted.next(), dotted.next())
                    {
                        // Use only the first two parts
                        literal = &literal[..one.len() + 1 + two.len()];
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
                    return new_token(TokenKind::Number(num), literal);
                }
                Started::Ident => todo!(),
            };
        }
    }
}
