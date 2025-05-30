use crate::expression::Expr;
use std::fmt::{Debug, Display};

use crate::Token;

#[derive(Debug)]
pub enum Stmt {
    Expression {
        expression: Box<Expr>,
    },
    Print {
        expression: Box<Expr>,
    },
    Var {
        name: Token,
        initializer: Box<Expr>,
    },
    Block {
        statements: Vec<Stmt>,
    },
    If {
        condition: Box<Expr>,
        then_stmt: Box<Stmt>,
        else_stmt: Option<Box<Stmt>>,
    },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression { expression } => write!(f, "Expression {{ {expression} }}"),
            Stmt::Print { expression } => write!(f, "Print {{ {expression} }}"),
            Stmt::Var { name, initializer } => write!(f, "var {name} {{ {initializer} }}"),
            Stmt::Block { statements } => write!(f, "Block {{ {statements:?} }}"),
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => write!(f, "If {condition} then {then_stmt:?} else {else_stmt:?}"),
        }
    }
}
