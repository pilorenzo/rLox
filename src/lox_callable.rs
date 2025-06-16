use core::fmt;
use std::fmt::{Debug, Display};

use crate::interpreter::Interpreter;
use crate::{
    environment::Environment, interpreter::RuntimeError, statement::FunctionDeclaration, Literal,
};

#[derive(Debug, Clone, PartialEq)]
pub enum LoxCallable {
    LoxAnonymous {
        arity: usize,
        func: fn(Vec<Literal>) -> Literal,
    },
    LoxFunction {
        declaration: FunctionDeclaration,
    },
}

impl LoxCallable {
    pub fn get_arity(&self) -> usize {
        match self {
            LoxCallable::LoxAnonymous { arity, func: _ } => *arity,
            LoxCallable::LoxFunction { declaration } => declaration.params.len(),
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, RuntimeError> {
        match self {
            LoxCallable::LoxAnonymous { arity: _, func } => Ok((func)(arguments)),
            LoxCallable::LoxFunction { declaration } => {
                interpreter.graph.envs.push(Environment::new());
                for (param, arg) in declaration.params.iter().zip(arguments.iter()) {
                    interpreter
                        .get_last_environment_mut()
                        .define(param.lexeme.clone(), arg.clone())
                }
                interpreter.execute_block(&declaration.body)?;
                Ok(Literal::Null)
            }
        }
    }
}

impl Display for LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxCallable::LoxAnonymous { arity: _, func: _ } => write!(f, "<anonymous function>"),
            LoxCallable::LoxFunction { declaration } => {
                let name = &declaration.name.lexeme;
                write!(f, "<fn {name}>")
            }
        }
    }
}
