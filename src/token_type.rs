use std::{
    cell::RefCell,
    fmt::{Debug, Display},
    hash::Hash,
    rc::Rc,
};

use crate::{
    interpreter::RuntimeError,
    lox_callable::{LoxCallable, LoxClass, LoxFunction, LoxInstance},
};

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

impl Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.t_type.hash(state);
        self.lexeme.hash(state);
        self.line.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Numeric(f64),
    Letters(String),
    Null,
    Boolean(bool),
    Callable(Box<LoxCallable>),
    Class(Rc<RefCell<LoxInstance>>),
}

impl Literal {
    pub fn is_truthy(&self) -> bool {
        match self {
            Literal::Null => false,
            Literal::Boolean(b) => *b,
            _ => true,
        }
    }
    pub fn to_num(&self, line: i32) -> Result<f64, RuntimeError> {
        match *self {
            Literal::Numeric(n) => Ok(n),
            _ => Err(RuntimeError::InvalidOperationError {
                line,
                msg: format!("Can't cast {} to numeric", self),
            }),
        }
    }

    // pub fn get_function(self) -> Option<LoxFunction> {
    //     match self {
    //         Literal::Callable(c) => match *c {
    //             LoxCallable::Function { function } => Some(function),
    //             _ => None,
    //         },
    //         _ => None,
    //     }
    // }

    pub fn new_function(function: LoxFunction) -> Self {
        Literal::Callable(Box::new(LoxCallable::Function { function }))
    }

    pub fn get_class(self) -> Option<LoxClass> {
        match self {
            Literal::Callable(c) => match *c {
                LoxCallable::Class { class } => Some(class),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn new_class(class: LoxClass) -> Self {
        Literal::Callable(Box::new(LoxCallable::Class { class }))
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let value = match self {
            Self::Numeric(n) => n.to_string(),
            Self::Letters(s) => s.clone(),
            Self::Null => "nil".to_owned(),
            Self::Boolean(b) => b.to_string(),
            Self::Callable(f) => f.to_string(),
            Self::Class(i) => i.borrow().to_string(),
        };
        write!(f, "{}", value)
    }
}

impl Hash for Literal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Literal::Numeric(f) => f.to_bits().hash(state),
            other => other.hash(state),
        }
    }
}
