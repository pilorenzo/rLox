use crate::expression::Expr;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::Token;

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}
impl PartialEq for FunctionDeclaration {
    fn eq(&self, other: &Self) -> bool {
        let has_same_name = self.name == other.name;
        let has_same_param = self.params == other.params;
        has_same_name && has_same_param
    }
}

impl Eq for FunctionDeclaration {}
impl Hash for FunctionDeclaration {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.params.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq)]
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
    While {
        condition: Box<Expr>,
        body: Box<Stmt>,
    },
    Fun {
        declaration: FunctionDeclaration,
    },
    Return {
        keyword: Token,
        value: Box<Expr>,
    },

    Class {
        name: Token,
        methods: Vec<FunctionDeclaration>,
        superclass: Option<Box<Expr>>,
    },
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression { expression } => write!(f, "Expression {{ {expression} }}"),
            Stmt::Print { expression } => write!(f, "Print {{ {expression} }}"),
            Stmt::Var { name, initializer } => write!(f, "var {name} {{ {initializer} }}"),
            Stmt::Block { statements } => {
                let mut formatted_stmts = String::default();
                for s in statements {
                    formatted_stmts += format!("\n{s}").as_str();
                }
                write!(f, "Block {{ {formatted_stmts}\n }}")
            }
            Stmt::Return { keyword: _, value } => write!(f, "Return {value}"),
            Stmt::While { condition, body } => write!(f, "While {condition} {{ {body} }}"),
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => match else_stmt {
                Some(else_stmt) => write!(f, "If {condition} then {then_stmt} else {else_stmt}"),
                None => write!(f, "If {condition} then {then_stmt}"),
            },
            Stmt::Fun { declaration } => {
                let (name, params) = (&declaration.name, &declaration.params);
                let mut formatted = String::default();
                for p in params {
                    formatted += format!("{p},").as_str();
                }
                let formatted_params = if let Some(i) = formatted.rfind(',') {
                    &formatted[..i]
                } else {
                    &formatted
                };
                // let formatted_params = &formatted[..formatted.rfind(',').unwrap()];
                write!(f, "Function {name} ({formatted_params})")
            }
            Stmt::Class {
                name,
                methods: _,
                superclass: _,
            } => write!(f, "Class {}", name.lexeme),
        }
    }
}
