use crate::{interpreter::RuntimeError, token_type::Token, Literal};
// use std::cell::RefCell;
use std::collections::HashMap;
// use std::rc::Rc;

pub struct Environment {
    dict: HashMap<String, Literal>,
}

impl Environment {
    pub fn global() -> Self {
        Environment {
            dict: HashMap::<String, Literal>::new(),
        }
    }
    pub fn new() -> Self {
        Environment {
            dict: HashMap::<String, Literal>::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.dict.insert(name, value);
    }
}

pub struct EnvironmentGraph {
    pub envs: Vec<Environment>,
}

impl EnvironmentGraph {
    pub fn new(global: Environment) -> Self {
        Self { envs: vec![global] }
    }

    pub fn get(&self, token: Token) -> Result<Literal, RuntimeError> {
        // self.get_in_env(token, &self.envs.last().unwrap())
        let name = &token.lexeme;
        for env in self.envs.iter().rev() {
            if env.dict.contains_key(name) {
                return Ok(env.dict[name].clone());
            }
        }

        Err(RuntimeError::IdentifierError {
            line: token.line,
            msg: format!("Undefined variable '{name}'."),
        })
    }

    pub fn assign(&mut self, token: Token, value: Literal) -> Result<(), RuntimeError> {
        let name = &token.lexeme;
        for env in self.envs.iter_mut().rev() {
            if let Some(v) = env.dict.get_mut(name) {
                *v = value;
                return Ok(());
            }
        }

        Err(RuntimeError::UndefinedVariable {
            line: token.line,
            msg: format!("Trying to assign to an undefined variable '{name}'."),
        })
    }
}
