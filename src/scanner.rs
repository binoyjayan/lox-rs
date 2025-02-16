use miette::{Error, LabeledSpan};

use crate::token::Token;

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
        let mut chars = self.rest.chars();
        let c = chars.next()?;
        self.rest = chars.as_str();
        self.byte += c.len_utf8();

        match c {
            '(' => return Some(Ok(Token::LeftParen)),
            ')' => return Some(Ok(Token::RightParen)),
            '{' => return Some(Ok(Token::LeftBrace)),
            '}' => return Some(Ok(Token::RightBrace)),
            ',' => return Some(Ok(Token::Comma)),
            '.' => return Some(Ok(Token::Dot)),
            '-' => return Some(Ok(Token::Minus)),
            '+' => return Some(Ok(Token::Plus)),
            ';' => return Some(Ok(Token::Semicolon)),
            '*' => return Some(Ok(Token::Star)),
            '"' => {}
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
        }
        self.byte += c.len_utf8();
        todo!()
    }
}
