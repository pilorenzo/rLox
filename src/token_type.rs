use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::literal::Literal;

#[derive(Debug, PartialEq, Clone, Copy, Hash)]
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

#[derive(Debug, Clone, PartialEq)]
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

    pub fn new_this(line: i32) -> Self {
        Token::new(TokenType::This, "this", Literal::Null, line)
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

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.t_type.hash(state);
        self.lexeme.hash(state);
        self.line.hash(state);
    }
}
