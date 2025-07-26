use crate::token_type::Literal;
use std::fmt::{Debug, Display};

use crate::Token;

#[derive(Debug, Clone, PartialEq, Hash)]
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
    Get {
        object: Box<Expr>,
        name: Token,
    },
    Set {
        object: Box<Expr>,
        name: Token,
        value: Box<Expr>,
    },
    This {
        keyword: Token,
    },
    Super {
        keyword: Token,
        method: Token,
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
            Expr::This { keyword } => write!(f, "Current class instance keyword {keyword}"),
            Expr::Super { keyword, method } => {
                write!(f, "Super class {keyword}, with method {method}")
            }
            Expr::Get { name, object: _ } => write!(f, "Get {name}"),
            Expr::Set {
                name,
                object: _,
                value,
            } => write!(f, "Set {name} to {value}"),
            Expr::Call {
                callee,
                paren: _,
                args,
            } => write!(f, "Call {{ {callee}, {args:?} }}"),
        }
    }
}

impl Eq for Expr {}

impl Expr {
    pub fn is_null(&self) -> bool {
        matches!(
            self,
            Expr::Literal {
                value: Literal::Null,
            }
        )
    }

    /*
     *   This is just a dirty trick used to resolve
     *   the iteration in a for loop without issue.
     *   consider the following:
     *   for(var i = 0; i < 2; i = i +1){...}
     *   since the condition and the increment are in
     *   the same line (and since I'm not using an id
     *   to identify the expression, which I definitely should)
     *   the resolver overwrites the interpreter.locals value associated
     *   with the key that correspond to i (that is, the distance to the scopes in
     *   which the local is).
     *   The problem is that the above "for"" is desugarised to
     *   {
     *       var i = 0;
     *       while(i < 2) {
     *           ...
     *           i = i + 1;
     *       }
     *   }
     *   and, since the condition and the increment are in different scopes,
     *   the interpreter can't find "i" when interpreting the condition.
     *   This method change the line of the condition so that 2 locals are
     *   stored in the interpreter's HashMap
     */
    pub fn decrement_line(&mut self) {
        match self {
            Expr::Assignment { name, value } => {
                name.line -= 1;
                value.decrement_line();
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                left.decrement_line();
                operator.line -= 1;
                right.decrement_line();
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                left.decrement_line();
                operator.line -= 1;
                right.decrement_line();
            }
            Expr::Grouping { expression } => expression.decrement_line(),
            Expr::Literal { value: _ } => {}
            Expr::Unary { operator, right } => {
                operator.line -= 1;
                right.decrement_line();
            }
            Expr::Variable { name } => name.line -= 1,
            Expr::Call {
                callee,
                paren,
                args,
            } => {
                callee.decrement_line();
                paren.line -= 1;
                for arg in args {
                    arg.decrement_line();
                }
            }
            Expr::Get { object, name } => {
                object.decrement_line();
                name.line -= 1;
            }
            Expr::Set {
                object,
                name,
                value,
            } => {
                object.decrement_line();
                name.line -= 1;
                value.decrement_line();
            }
            Expr::This { keyword } => keyword.line -= 1,
            Expr::Super { keyword, method } => {
                keyword.line -= 1;
                method.line -= 1;
            }
        }
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
