use crate::{expression::Expr, token_type::Literal};
use std::fmt::{Debug, Display};

use crate::Token;

#[derive(Debug)]
pub enum Stmt {
    Expression { expression: Box<Expr> },
    Print { expression: Box<Expr> },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression { expression } => write!(f, "Expression {{ {expression} }}"),
            Stmt::Print { expression } => write!(f, "Print {{ {expression} }}"),
        }
    }
}
