use crate::lox_callable::{LoxCallable, LoxClass, LoxFunction, LoxInstance};
use crate::RuntimeError;
use std::cell::RefCell;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;

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
            _ => Err(RuntimeError::Error {
                line,
                msg: format!("Can't cast {} to numeric", self),
            }),
        }
    }

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
