use core::fmt;
use std::fmt::{Debug, Display};

use crate::{
    environment::Environment,
    interpreter::{execute_block, Interpreter},
    statement::FunctionDeclaration,
    Literal,
};

/* TODO: make LoxCallable an enum, it seems like it's way easier */

pub trait LoxCallable: fmt::Display + PartialEq {
    fn get_arity(&self) -> usize;
    fn call(&self, env: Option<Environment>, arguments: Vec<Literal>) -> Literal;
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxAnonymous {
    arity: usize,
    pub func: fn(Vec<Literal>) -> Literal,
}

impl LoxAnonymous {
    pub fn new(arity: usize, func: fn(Vec<Literal>) -> Literal) -> Self {
        Self { arity, func }
    }
}

impl LoxCallable for LoxAnonymous {
    fn get_arity(&self) -> usize {
        self.arity
    }
    fn call(&self, _: Option<Environment>, arguments: Vec<Literal>) -> Literal {
        (self.func)(arguments)
    }
}

impl Display for LoxAnonymous {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<anonymous function>")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFunction {
    declaration: FunctionDeclaration,
}

impl LoxFunction {
    pub fn new(declaration: FunctionDeclaration) -> Self {
        Self { declaration }
    }
}

impl LoxCallable for LoxFunction {
    fn get_arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(&self, global: Option<Environment>, arguments: Vec<Literal>) -> Literal {
        let mut environment =
            Environment::new(global.expect("LoxFunction without global environment"));
        for (param, arg) in self.declaration.params.iter().zip(arguments.iter()) {
            environment.define(param.lexeme.clone(), arg.clone())
        }
        execute_block(&self.declaration.body, environment);
        Literal::Null
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = &self.declaration.name.lexeme;
        write!(f, "<fn {name}>")
    }
}
