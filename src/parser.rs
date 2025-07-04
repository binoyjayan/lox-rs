use miette::{Context, Error, LabeledSpan};
use std::fmt;
use std::fmt::Display;

use crate::scanner::Scanner;
use crate::token::{Token, TokenKind};

pub struct Parser<'de> {
    whole: &'de str,
    scanner: Scanner<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(input: &'de str) -> Self {
        Self {
            whole: input,
            scanner: Scanner::new(input),
        }
    }

    pub fn parse(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_stmt_within(0)
    }

    pub fn parse_expr(mut self) -> Result<TokenTree<'de>, Error> {
        self.parse_expr_within(0)
    }

    pub fn parse_block(&mut self) -> Result<TokenTree<'de>, Error> {
        self.scanner.expect(TokenKind::LeftBrace, "missing '{'")?;

        let body = self.parse_stmt_within(0)?;

        self.scanner
            .expect(TokenKind::RightBrace, "missing '}'")
            .wrap_err("after block body")?;

        Ok(body)
    }

    pub fn parse_call_args(&mut self) -> Result<Vec<TokenTree<'de>>, Error> {
        let mut arguments = Vec::new();
        if matches!(
            self.scanner.peek(),
            Some(Ok(Token {
                kind: TokenKind::RightParen,
                ..
            }))
        ) {
            // immediate argument list end
        } else {
            loop {
                let argument = self.parse_expr_within(0).wrap_err_with(|| {
                    format!("in argument #{} of function call", arguments.len() + 1)
                })?;
                arguments.push(argument);

                let token = self
                    .scanner
                    .expect_where(
                        |token| matches!(token.kind, TokenKind::RightParen | TokenKind::Comma),
                        "continuing argument list",
                    )
                    .wrap_err("in argument list of function call")?;

                if token.kind == TokenKind::RightParen {
                    break;
                }
            }
        }

        Ok(arguments)
    }

    pub fn parse_stmt_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.scanner.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };

        let mut lhs = match lhs {
            Token {
                kind: TokenKind::Ident,
                ..
            } => TokenTree::Atom(Atom::Ident(lhs.literal)),
            Token {
                kind: TokenKind::Super,
                ..
            } => TokenTree::Atom(Atom::Super),
            Token {
                kind: TokenKind::This,
                ..
            } => TokenTree::Atom(Atom::This),
            // Groups
            Token {
                kind: TokenKind::LeftParen,
                ..
            } => {
                let lhs = self
                    .parse_expr_within(0)
                    .wrap_err("in bracketed expression")?;

                // Expect the closing parenthesis
                self.scanner
                    .expect(
                        TokenKind::RightParen,
                        "Unexpected end to bracketed expression",
                    )
                    .wrap_err("after bracketed expression")?;
                TokenTree::Cons(Op::Group, vec![lhs])
            }
            // Unary prefix expressions
            Token {
                kind: TokenKind::Print | TokenKind::Return,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Print => Op::Print,
                    TokenKind::Return => Op::Return,
                    _ => unreachable!("by the outer match arm pattern"),
                };
                let ((), r_bp) = prefix_binding_power(&op);
                let rhs = self.parse_expr_within(r_bp).wrap_err("in RHS")?;
                return Ok(TokenTree::Cons(op, vec![rhs]));
            }

            Token {
                kind: TokenKind::For,
                ..
            } => {
                self.scanner
                    .expect(TokenKind::LeftParen, "missing '('")
                    .wrap_err("after for keyword")?;

                let init = self
                    .parse_expr_within(0)
                    .wrap_err(format!("in first argument of for"))?;

                self.scanner
                    .expect(TokenKind::Semicolon, "missing ';'")
                    .wrap_err("after first argument of for")?;

                let condition = self
                    .parse_expr_within(0)
                    .wrap_err(format!("in second argument of for"))?;

                self.scanner
                    .expect(TokenKind::Semicolon, "missing ';'")
                    .wrap_err("after second argument of for")?;

                let step = self
                    .parse_expr_within(0)
                    .wrap_err(format!("in third argument of for"))?;

                self.scanner
                    .expect(TokenKind::RightParen, "missing ')'")
                    .wrap_err("after for arguments")?;

                let block = self.parse_block().wrap_err("in for loop")?;

                return Ok(TokenTree::Cons(Op::For, vec![init, condition, step, block]));
            }

            Token {
                kind: TokenKind::While,
                ..
            } => {
                self.scanner
                    .expect(TokenKind::LeftParen, "missing '('")
                    .wrap_err("after while keyword")?;

                let condition = self
                    .parse_expr_within(0)
                    .wrap_err(format!("in second argument of while"))?;

                self.scanner
                    .expect(TokenKind::RightParen, "missing ')'")
                    .wrap_err("after while condition")?;

                let block = self.parse_block().wrap_err("in while loop")?;

                return Ok(TokenTree::Cons(Op::While, vec![condition, block]));
            }

            Token {
                kind: TokenKind::Class,
                ..
            } => {
                let token = self
                    .scanner
                    .expect(TokenKind::Ident, "missing identifier")
                    .wrap_err("after class name")?;

                let ident = match token.kind {
                    TokenKind::Ident => Atom::Ident(token.literal),
                    _ => unreachable!("by the outer match arm pattern"),
                };

                let body = self.parse_block().wrap_err("in class body")?;
                return Ok(TokenTree::Cons(
                    Op::Class,
                    vec![TokenTree::Atom(ident), body],
                ));
            }

            Token {
                kind: TokenKind::Var,
                ..
            } => {
                let token = self
                    .scanner
                    .expect(TokenKind::Ident, "missing identifier")
                    .wrap_err("in var declaration")?;

                let ident = match token.kind {
                    TokenKind::Ident => TokenTree::Atom(Atom::Ident(token.literal)),
                    _ => unreachable!("by the outer match arm pattern"),
                };

                // Expected an `=`
                self.scanner
                    .expect(TokenKind::Equal, "missing '='")
                    .wrap_err("in variable declaration")?;

                let expr = self.parse_expr_within(0).wrap_err("in var declaration")?;
                return Ok(TokenTree::Cons(Op::Var, vec![ident, expr]));
            }

            Token {
                kind: TokenKind::Fun,
                ..
            } => {
                let token = self
                    .scanner
                    .expect(TokenKind::Ident, "expected identifier")
                    .wrap_err("in function name declaration")?;
                assert_eq!(token.kind, TokenKind::Ident);
                let name = token.literal;
                let ident = Atom::Ident(token.literal);

                let mut parameters = Vec::new();

                self.scanner
                    .expect(TokenKind::LeftParen, "missing (")
                    .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                if matches!(
                    self.scanner.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::RightParen,
                        ..
                    }))
                ) {
                    // immediate parameter list end
                } else {
                    loop {
                        let parameter = self
                            .scanner
                            .expect(TokenKind::Ident, "unexpected token")
                            .wrap_err_with(|| {
                            format!("in parameter #{} of function {name}", parameters.len() + 1)
                        })?;
                        parameters.push(parameter);

                        let token = self
                            .scanner
                            .expect_where(
                                |token| {
                                    matches!(token.kind, TokenKind::RightParen | TokenKind::Comma)
                                },
                                "continuing parameter list",
                            )
                            .wrap_err_with(|| format!("in parameter list of function {name}"))?;

                        if token.kind == TokenKind::RightParen {
                            break;
                        }
                    }
                }

                let body = self.parse_block().wrap_err("in function body")?;

                return Ok(TokenTree::Fun {
                    name: ident,
                    params: parameters,
                    body: Box::new(body),
                });
            }

            // Prefix, if expression
            Token {
                kind: TokenKind::If,
                ..
            } => {
                self.scanner
                    .expect(TokenKind::LeftParen, "missing '('")
                    .wrap_err("after if keyword")?;

                let condition = self
                    .parse_expr_within(0)
                    .wrap_err(format!("in if condition"))?;

                self.scanner
                    .expect(TokenKind::RightParen, "missing ')'")
                    .wrap_err("after if condition")?;

                let if_block = self.parse_block().wrap_err("in if block")?;

                let else_branch = if matches!(
                    self.scanner.peek(),
                    Some(Ok(Token {
                        kind: TokenKind::Else,
                        ..
                    }))
                ) {
                    self.scanner.next();
                    Some(Box::new(self.parse_block().wrap_err("in else block")?))
                } else {
                    None
                };

                return Ok(TokenTree::If {
                    condition: Box::new(condition),
                    then_branch: Box::new(if_block),
                    else_branch,
                });
            }
            token => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    token.offset..token.offset + token.literal.len(),
                    "unexpected token in statement"
                )],
                help = format!("Unexpected  token: {token:?}"),
                "Expected a statement",
            )
            .with_source_code(self.whole.to_string()))?,
        };
        loop {
            let op = self.scanner.peek();
            if op.map_or(false, |t| t.is_err()) {
                return Err(self
                    .scanner
                    .next()
                    .expect("checked some above")
                    .expect_err("checked error above"))
                .wrap_err("in expected operator");
            }
            let op = match op.map(|res| res.as_ref().expect("handled error above")) {
                None => break,
                Some(Token {
                    kind: TokenKind::LeftParen,
                    ..
                }) => Op::Call,
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => Op::Field,
                Some(token) => return Err(miette::miette! {
                    labels = vec![
                        LabeledSpan::at(token.offset..token.offset + token.literal.len(), "here"),
                    ],
                    help = format!("Unexpected {token:?}"),
                    "Expected an operator",
                }
                .with_source_code(self.whole.to_string())),
            };
            if let Some((l_bp, ())) = postfix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.scanner.next();
                lhs = match op {
                    Op::Call => TokenTree::Call {
                        callee: Box::new(lhs),
                        args: self
                            .parse_call_args()
                            .wrap_err("in function call arguments")?,
                    },
                    _ => TokenTree::Cons(op, vec![lhs]),
                };
                continue;
            }
            if let Some((l_bp, r_bp)) = infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.scanner.next();
                let rhs = self.parse_expr_within(r_bp).wrap_err("on the rhs")?;
                lhs = TokenTree::Cons(op, vec![lhs, rhs]);

                continue;
            }
            break;
        }
        Ok(lhs)
    }

    pub fn parse_expr_within(&mut self, min_bp: u8) -> Result<TokenTree<'de>, Error> {
        let lhs = match self.scanner.next() {
            Some(Ok(token)) => token,
            None => return Ok(TokenTree::Atom(Atom::Nil)),
            Some(Err(e)) => {
                return Err(e).wrap_err("on left-hand side");
            }
        };

        let mut lhs = match lhs {
            // Atoms
            Token {
                kind: TokenKind::Nil,
                ..
            } => TokenTree::Atom(Atom::Nil),
            Token {
                kind: TokenKind::String,
                literal,
                ..
            } => TokenTree::Atom(Atom::String(Token::unescape(literal))),
            Token {
                kind: TokenKind::Number(n),
                ..
            } => TokenTree::Atom(Atom::Number(n)),
            Token {
                kind: TokenKind::True,
                ..
            } => TokenTree::Atom(Atom::Bool(true)),
            Token {
                kind: TokenKind::False,
                ..
            } => TokenTree::Atom(Atom::Bool(false)),
            Token {
                kind: TokenKind::Ident,
                ..
            } => TokenTree::Atom(Atom::Ident(lhs.literal)),
            Token {
                kind: TokenKind::Super,
                ..
            } => TokenTree::Atom(Atom::Super),
            Token {
                kind: TokenKind::This,
                ..
            } => TokenTree::Atom(Atom::This),

            // Group expressions
            Token {
                kind: TokenKind::LeftParen,
                ..
            } => {
                // Parse the content of the group
                let lhs = self
                    .parse_expr_within(0)
                    .wrap_err("in bracketed expression")?;

                // Expect the closing bracket
                self.scanner
                    .expect(
                        TokenKind::RightParen,
                        "Unexpected end to bracketed expression",
                    )
                    .wrap_err("after bracketed expression")?;
                TokenTree::Cons(Op::Group, vec![lhs])
            }

            // Unary prefix expressions
            Token {
                kind: TokenKind::Bang | TokenKind::Minus,
                ..
            } => {
                let op = match lhs.kind {
                    TokenKind::Bang => Op::Bang,
                    TokenKind::Minus => Op::Minus,
                    _ => unreachable!("by the outer match arm pattern"),
                };
                let ((), r_bp) = prefix_binding_power(&op);
                let rhs = self.parse_expr_within(r_bp).wrap_err("in RHS")?;
                TokenTree::Cons(op, vec![rhs])
            }

            token => Err(miette::miette!(
                labels = vec![LabeledSpan::at(
                    token.offset..token.offset + token.literal.len(),
                    "unexpected token in expression"
                )],
                help = format!("Unexpected  token: {token:?}"),
                "Expected an expression",
            )
            .with_source_code(self.whole.to_string()))?,
        };
        loop {
            let op = self.scanner.peek();
            if op.map_or(false, |t| t.is_err()) {
                return Err(self
                    .scanner
                    .next()
                    .expect("checked some above")
                    .expect_err("checked error above"))
                .wrap_err("in place of expected operator");
            }
            let op = match op.map(|res| res.as_ref().expect("handled error above")) {
                None => break,
                Some(Token {
                    kind:
                        TokenKind::RightParen
                        | TokenKind::RightBrace
                        | TokenKind::Comma
                        | TokenKind::Semicolon,
                    ..
                }) => {
                    break;
                }
                Some(Token {
                    kind: TokenKind::LeftParen,
                    ..
                }) => Op::Call,
                Some(Token {
                    kind: TokenKind::Dot,
                    ..
                }) => Op::Field,
                Some(Token {
                    kind: TokenKind::Minus,
                    ..
                }) => Op::Minus,
                Some(Token {
                    kind: TokenKind::Plus,
                    ..
                }) => Op::Plus,
                Some(Token {
                    kind: TokenKind::Star,
                    ..
                }) => Op::Star,
                Some(Token {
                    kind: TokenKind::Slash,
                    ..
                }) => Op::Slash,
                Some(Token {
                    kind: TokenKind::BangEqual,
                    ..
                }) => Op::BangEqual,
                Some(Token {
                    kind: TokenKind::EqualEqual,
                    ..
                }) => Op::EqualEqual,
                Some(Token {
                    kind: TokenKind::GreaterEqual,
                    ..
                }) => Op::GreaterEqual,
                Some(Token {
                    kind: TokenKind::LessEqual,
                    ..
                }) => Op::LessEqual,
                Some(Token {
                    kind: TokenKind::Greater,
                    ..
                }) => Op::Greater,
                Some(Token {
                    kind: TokenKind::Less,
                    ..
                }) => Op::Less,
                Some(Token {
                    kind: TokenKind::And,
                    ..
                }) => Op::And,
                Some(Token {
                    kind: TokenKind::Or,
                    ..
                }) => Op::Or,
                Some(token) => {
                    return Err(miette::miette!(
                        labels = vec![LabeledSpan::at(
                            token.offset..token.offset + token.literal.len(),
                            "unexpected token in statement"
                        )],
                        help = format!("Unexpected  token: {token:?}"),
                        "Expected a statement",
                    )
                    .with_source_code(self.whole.to_string()))?;
                }
            };
            if let Some((l_bp, ())) = postfix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }

                self.scanner.next();
                lhs = match op {
                    Op::Call => {
                        let args = self
                            .parse_call_args()
                            .wrap_err(format!("in function call arg list"))?;
                        TokenTree::Call {
                            callee: Box::new(lhs),
                            args,
                        }
                    }
                    _ => TokenTree::Cons(op, vec![lhs]),
                };
                continue;
            }
            if let Some((l_bp, r_bp)) = infix_binding_power(&op) {
                if l_bp < min_bp {
                    break;
                }
                self.scanner.next();
                lhs = match op {
                    // TODO: Ternary operator
                    _ => {
                        let rhs = self.parse_expr_within(r_bp).wrap_err("on the rhs")?;
                        TokenTree::Cons(op, vec![lhs, rhs])
                    }
                };
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom<'de> {
    Nil,
    Bool(bool),
    Number(f64),
    String(std::borrow::Cow<'de, str>),
    Ident(&'de str),
    Super,
    This,
}

impl Display for Atom<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Atom::Nil => write!(f, "nil"),
            Atom::Bool(b) => write!(f, "{}", b),
            Atom::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "{}.0", n)
                } else {
                    write!(f, "{}", n)
                }
            }
            Atom::String(s) => write!(f, "\"{}\"", s),
            Atom::Ident(i) => write!(f, "{}", i),
            Atom::Super => write!(f, "super"),
            Atom::This => write!(f, "this"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]

pub enum Op {
    Plus,
    Minus,
    Star,
    Slash,
    Bang,
    BangEqual,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    For,
    Call,
    Class,
    Print,
    Return,
    Field,
    Var,
    While,
    Group,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Plus => write!(f, "+"),
            Op::Minus => write!(f, "-"),
            Op::Star => write!(f, "*"),
            Op::Slash => write!(f, "/"),
            Op::Bang => write!(f, "!"),
            Op::BangEqual => write!(f, "!="),
            Op::EqualEqual => write!(f, "=="),
            Op::Less => write!(f, "<"),
            Op::LessEqual => write!(f, "<="),
            Op::Greater => write!(f, ">"),
            Op::GreaterEqual => write!(f, ">="),
            Op::And => write!(f, "and"),
            Op::Or => write!(f, "or"),
            Op::For => write!(f, "for"),
            Op::Call => write!(f, "call"),
            Op::Class => write!(f, "class"),
            Op::Print => write!(f, "print"),
            Op::Return => write!(f, "return"),
            Op::Field => write!(f, "."),
            Op::Var => write!(f, "var"),
            Op::While => write!(f, "while"),
            Op::Group => write!(f, "group"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree<'de> {
    Atom(Atom<'de>),
    Cons(Op, Vec<TokenTree<'de>>),
    Fun {
        name: Atom<'de>,
        params: Vec<Token<'de>>,
        body: Box<TokenTree<'de>>,
    },
    Call {
        callee: Box<TokenTree<'de>>,
        args: Vec<TokenTree<'de>>,
    },
    If {
        condition: Box<TokenTree<'de>>,
        then_branch: Box<TokenTree<'de>>,
        else_branch: Option<Box<TokenTree<'de>>>,
    },
}

impl fmt::Display for TokenTree<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenTree::Atom(i) => write!(f, "{}", i),
            TokenTree::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?
                }
                write!(f, ")")
            }
            TokenTree::Fun { name, params, body } => {
                write!(f, "fun {}(", name)?;
                for (i, param) in params.iter().enumerate() {
                    write!(f, "{}", param)?;
                    if i < params.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ") {}", body)
            }
            TokenTree::If {
                condition,
                then_branch,
                else_branch,
            } => {
                write!(f, "if {} {}", condition, then_branch)?;
                if let Some(else_branch) = else_branch {
                    write!(f, " else {}", else_branch)?;
                }
                Ok(())
            }
            TokenTree::Call { callee, args } => {
                write!(f, "{}", callee)?;
                for arg in args {
                    write!(f, ", {}", arg)?;
                }
                write!(f, ")")
            }
        }
    }
}

fn prefix_binding_power(op: &Op) -> ((), u8) {
    match op {
        Op::Print | Op::Return => ((), 1),
        Op::Bang | Op::Minus => ((), 11),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: &Op) -> Option<(u8, ())> {
    let res = match op {
        Op::Call => (13, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: &Op) -> Option<(u8, u8)> {
    let res = match op {
        Op::And | Op::Or => (3, 4),
        Op::BangEqual
        | Op::EqualEqual
        | Op::Less
        | Op::LessEqual
        | Op::Greater
        | Op::GreaterEqual => (5, 6),
        Op::Plus | Op::Minus => (7, 8),
        Op::Star | Op::Slash => (9, 10),
        Op::Field => (16, 15),
        _ => return None,
    };
    Some(res)
}
