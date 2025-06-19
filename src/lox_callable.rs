use core::fmt;
use std::cell::RefCell;
use std::fmt::{Debug, Display};
use std::rc::Rc;

use crate::environment::EnvironmentNode;
use crate::interpreter::Interpreter;
use crate::{
    environment::Environment, interpreter::RuntimeError, statement::FunctionDeclaration, Literal,
};

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFunction {
    declaration: FunctionDeclaration,
    closure: Rc<RefCell<Environment>>,
}
impl LoxFunction {
    pub fn new(declaration: FunctionDeclaration, closure: Rc<RefCell<Environment>>) -> Self {
        Self {
            declaration,
            closure,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoxCallable {
    Anonymous {
        arity: usize,
        func: fn(Vec<Literal>) -> Literal,
    },
    Function {
        function: LoxFunction,
    },
}

impl LoxCallable {
    pub fn get_arity(&self) -> usize {
        match self {
            LoxCallable::Anonymous { arity, func: _ } => *arity,
            LoxCallable::Function { function } => function.declaration.params.len(),
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, RuntimeError> {
        match self {
            LoxCallable::Anonymous { arity: _, func } => Ok((func)(arguments)),
            LoxCallable::Function { function } => {
                let mut is_closure_env_in_graph = false;
                let outer = interpreter.graph.envs.last();
                if let Some(EnvironmentNode::Closure { env }) = outer {
                    is_closure_env_in_graph = env == &function.closure;
                }
                if !is_closure_env_in_graph {
                    interpreter.graph.envs.push(EnvironmentNode::Closure {
                        env: Rc::clone(&function.closure),
                    });
                }
                interpreter.graph.push(Environment::new());
                for (param, arg) in function.declaration.params.iter().zip(arguments.iter()) {
                    interpreter.graph.define(param.lexeme.clone(), arg.clone())
                }
                let res = match interpreter.execute_block(&function.declaration.body) {
                    Err(RuntimeError::Return { value }) => Ok(value),
                    Err(e) => Err(e),
                    Ok(()) => Ok(Literal::Null),
                };
                interpreter.graph.envs.pop();
                if !is_closure_env_in_graph {
                    interpreter.graph.envs.pop();
                }
                res
            }
        }
    }
}

impl Display for LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoxCallable::Anonymous { arity: _, func: _ } => write!(f, "<anonymous function>"),
            LoxCallable::Function { function } => {
                let name = &function.declaration.name.lexeme;
                write!(f, "<fn {name}>")
            }
        }
    }
}
