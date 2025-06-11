use std::fmt::Debug;

use crate::{interpreter::Interpreter, Literal};

#[derive(Debug, Clone, PartialEq)]
pub struct LoxCallable {
    pub arity: usize,
    pub func: fn(Vec<Literal>) -> Literal,
}

impl LoxCallable {
    pub fn arity(&self) -> usize {
        self.arity
    }
    pub fn call(&self, /*interpreter: Interpreter,*/ arguments: Vec<Literal>) -> Literal {
        (self.func)(arguments)
    }
}
