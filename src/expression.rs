use crate::token_type::Literal;
use std::fmt::{Debug, Display};

use crate::Token;

#[derive(Debug, Clone)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: Literal,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Assignment {
        name: Token,
        value: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        args: Vec<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary {
                left,
                operator,
                right,
            } => write!(f, "Binary {{ {left}, {operator}, {right} }}"),

            Expr::Logical {
                left,
                operator,
                right,
            } => write!(f, "Logical {{ {left}, {operator}, {right} }}"),

            Expr::Grouping { expression } => write!(f, "Grouping {{ {expression} }}"),
            Expr::Literal { value } => write!(f, "{value}"),
            Expr::Unary { operator, right } => write!(f, "Literal {{ {operator}, {right} }}"),
            Expr::Variable { name } => write!(f, "Variable {name}"),
            Expr::Assignment { name, value } => write!(f, "Assignment of {value} to {name}"),
            Expr::Call {
                callee,
                paren: _,
                args,
            } => write!(f, "Call {{ {callee}, {args:?} }}"),
        }
    }
}

impl Expr {
    pub fn is_null(&self) -> bool {
        matches!(
            self,
            Expr::Literal {
                value: Literal::Null,
            }
        )
    }
}
//     if let Expr::Literal { value } = self {
//         if let Literal::Null = value {
//             true
//         } else {
//             false
//         }
//     } else {
//         false
//     }
// }
