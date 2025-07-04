#[derive(Debug, Clone, PartialEq)]
pub struct Token<'de> {
    pub literal: &'de str,
    pub kind: TokenKind,
    pub offset: usize,
}

impl<'de> Token<'de> {
    pub fn new(literal: &'de str, kind: TokenKind, offset: usize) -> Self {
        Self {
            literal,
            kind,
            offset,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Bang,
    Equal,
    BangEqual,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Slash,
    String,
    Ident,
    Number(f64),
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Print,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = self.literal;
        match self.kind {
            TokenKind::LeftParen => write!(f, "LEFT_PAREN {literal} null"),
            TokenKind::RightParen => write!(f, "RIGHT_PAREN {literal} null"),
            TokenKind::LeftBrace => write!(f, "LEFT_BRACE {literal} null"),
            TokenKind::RightBrace => write!(f, "RIGHT_BRACE {literal} null"),
            TokenKind::Comma => write!(f, "COMMA {literal} null"),
            TokenKind::Dot => write!(f, "DOT {literal} null"),
            TokenKind::Minus => write!(f, "MINUS {literal} null"),
            TokenKind::Plus => write!(f, "PLUS {literal} null"),
            TokenKind::Semicolon => write!(f, "SEMICOLON {literal} null"),
            TokenKind::Star => write!(f, "STAR {literal} null"),
            TokenKind::Bang => write!(f, "BANG {literal} null"),
            TokenKind::Equal => write!(f, "EQUAL {literal} null"),
            TokenKind::BangEqual => write!(f, "BANG_EQUAL {literal} null"),
            TokenKind::EqualEqual => write!(f, "EQUAL_EQUAL {literal} null"),
            TokenKind::Less => write!(f, "LESS {literal} null"),
            TokenKind::LessEqual => write!(f, "LESS_EQUAL {literal} null"),
            TokenKind::Greater => write!(f, "GREATER {literal} null"),
            TokenKind::GreaterEqual => write!(f, "GREATER_EQUAL {literal} null"),
            TokenKind::Slash => write!(f, "SLASH {literal} null"),
            TokenKind::String => write!(f, "STRING {literal} {}", Token::unescape(literal)),
            TokenKind::Ident => write!(f, "IDENTIFIER {literal} null"),
            TokenKind::Print => write!(f, "PRINT {literal} null"),
            TokenKind::Number(n) => {
                if n.fract() == 0.0 {
                    write!(f, "NUMBER {literal} {:.1}", n)
                } else {
                    write!(f, "NUMBER {literal} {}", n)
                }
            }
            TokenKind::And => write!(f, "AND {literal} null"),
            TokenKind::Class => write!(f, "CLASS {literal} null"),
            TokenKind::Else => write!(f, "ELSE {literal} null"),
            TokenKind::False => write!(f, "FALSE {literal} null"),
            TokenKind::For => write!(f, "FOR {literal} null"),
            TokenKind::Fun => write!(f, "FUN {literal} null"),
            TokenKind::If => write!(f, "IF {literal} null"),
            TokenKind::Nil => write!(f, "NIL {literal} null"),
            TokenKind::Or => write!(f, "OR {literal} null"),
            TokenKind::Return => write!(f, "RETURN {literal} null"),
            TokenKind::Super => write!(f, "SUPER {literal} null"),
            TokenKind::This => write!(f, "THIS {literal} null"),
            TokenKind::True => write!(f, "TRUE {literal} null"),
            TokenKind::Var => write!(f, "VAR {literal} null"),
            TokenKind::While => write!(f, "WHILE {literal} null"),
        }
    }
}

impl Token<'_> {
    pub fn unescape<'de>(s: &'de str) -> std::borrow::Cow<'de, str> {
        // Since string have no escaping, they can't contain ", so trim won't trim multiple
        let s = s.trim_matches('"');
        std::borrow::Cow::Borrowed(s)
    }
}
