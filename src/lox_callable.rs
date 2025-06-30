use core::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ptr::{self, NonNull};
use std::rc::Rc;

use crate::environment::EnvironmentNode;
use crate::interpreter::Interpreter;
use crate::token_type::Token;
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

impl Hash for LoxFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.declaration.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub struct ClassName(pub String);

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum LoxCallable {
    Anonymous {
        arity: usize,
        func: fn(Vec<Literal>) -> Literal,
    },
    Function {
        function: LoxFunction,
    },
    Class {
        name: ClassName,
    },
}

impl LoxCallable {
    pub fn get_arity(&self) -> usize {
        match self {
            LoxCallable::Anonymous { arity, func: _ } => *arity,
            LoxCallable::Function { function } => function.declaration.params.len(),
            LoxCallable::Class { name: _ } => 0,
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
                    is_closure_env_in_graph = ptr::eq(&*env.borrow(), &*function.closure.borrow());
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
                // println!("\n\n###########################");
                // print!("Interpreter \n{interpreter}");
                // println!("---------------------------");
                // print!("Closure env :\n{}", function.closure.borrow());
                // println!("###########################\n\n");
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
            LoxCallable::Class { name } => Ok(Literal::Class(Box::new(LoxInstance::new(
                name.clone(),
                self.clone(),
            )))),
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
            LoxCallable::Class { name } => write!(f, "class {}", name.0),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxInstance {
    klass_name: ClassName,
    klass: LoxCallable,
    fields: HashMap<String, Literal>,
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.klass_name.0)
    }
}

impl LoxInstance {
    fn new(klass_name: ClassName, klass: LoxCallable) -> Self {
        Self {
            klass_name,
            klass,
            fields: Default::default(),
        }
    }

    pub fn get(&self, name: &Token) -> Result<Literal, RuntimeError> {
        match self.fields.get(&name.lexeme) {
            Some(l) => Ok(l.clone()),
            None => Err(RuntimeError::PropertyError {
                line: name.line,
                msg: format!("undefined property '{}'.", name.lexeme),
            }),
        }
    }

    pub fn set(&mut self, name: &Token, value: &Literal) {
        self.fields.insert(name.lexeme.clone(), value.clone());
    }
}
