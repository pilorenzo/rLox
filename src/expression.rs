use crate::token_type::LiteralType;
use std::fmt::Debug;

use crate::Token;

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Option<LiteralType>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
}
