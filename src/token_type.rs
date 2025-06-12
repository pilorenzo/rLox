use std::fmt::{Debug, Display};

use crate::lox_callable::LoxCallable;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    Chars,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub t_type: TokenType,
    pub lexeme: String,
    pub literal: Literal,
    pub line: i32,
}

impl Token {
    pub fn new(t: TokenType, lex: &str, lit: Literal, line: i32) -> Self {
        Self {
            t_type: t,
            lexeme: lex.to_owned(),
            literal: lit.clone(),
            line,
        }
    }

    pub fn get_keyword_by_name(name: &str) -> Option<TokenType> {
        match name {
            "and" => Some(TokenType::And),
            "class" => Some(TokenType::Class),
            "else" => Some(TokenType::Else),
            "false" => Some(TokenType::False),
            "fun" => Some(TokenType::Fun),
            "for" => Some(TokenType::For),
            "if" => Some(TokenType::If),
            "nil" => Some(TokenType::Nil),
            "or" => Some(TokenType::Or),
            "print" => Some(TokenType::Print),
            "return" => Some(TokenType::Return),
            "super" => Some(TokenType::Super),
            "this" => Some(TokenType::This),
            "true" => Some(TokenType::True),
            "var" => Some(TokenType::Var),
            "while" => Some(TokenType::While),
            _ => None,
        }
    }

    pub fn empty_literal() -> Literal {
        Literal::Null
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token {{ {:?}, '{}', literal: {}, line: {} }} ",
            self.t_type, self.lexeme, self.literal, self.line
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Numeric(f64),
    Letters(String),
    Null,
    Boolean(bool),
    Callable(Box<dyn LoxCallable>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self {
            Self::Numeric(n) => n.to_string(),
            Self::Letters(s) => s.clone(),
            Self::Null => "nil".to_owned(),
            Self::Boolean(b) => b.to_string(),
            Self::Callable(f) => f.to_string(),
        };
        write!(f, "{}", value)
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct Callee {
//     pub name: String,
//     pub args: Vec<Literal>,
// }
