use crate::{interpreter::RuntimeError, token_type::Token, Literal};
use std::collections::HashMap;

pub struct Environment {
    dict: HashMap<String, Literal>,
    pub outer: Option<Box<Environment>>,
}

impl Environment {
    pub fn global() -> Self {
        Environment {
            dict: HashMap::<String, Literal>::new(),
            outer: None,
        }
    }
    pub fn new(outer: Environment) -> Self {
        Environment {
            dict: HashMap::<String, Literal>::new(),
            outer: Some(Box::new(outer)),
        }
    }
    pub fn define(&mut self, name: String, value: Literal) {
        self.dict.insert(name, value);
    }
    pub fn get(&self, token: Token) -> Result<Literal, RuntimeError> {
        let name = &token.lexeme;
        if self.dict.contains_key(name) {
            Ok(self.dict[name].clone())
        } else if let Some(outer) = &self.outer {
            outer.get(token)
        } else {
            Err(RuntimeError::IdentifierError {
                line: token.line,
                msg: format!("Undefined variable '{name}'."),
            })
        }
    }
    pub fn assign(&mut self, token: Token, value: Literal) -> Result<(), RuntimeError> {
        let name = &token.lexeme;

        if let Some(v) = self.dict.get_mut(name) {
            *v = value;
            Ok(())
        } else if let Some(outer) = &mut self.outer {
            outer.assign(token, value)
        } else {
            Err(RuntimeError::UndefinedVariable {
                line: token.line,
                msg: format!("Trying to assign to an undefined variable '{name}'."),
            })
        }
    }
}
