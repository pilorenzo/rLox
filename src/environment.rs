use crate::{interpreter::RuntimeError, token_type::Token, Literal};
use std::collections::HashMap;

pub struct Environment(HashMap<String, Literal>);

impl Environment {
    pub fn new() -> Self {
        Environment(HashMap::<String, Literal>::new())
    }
    pub fn define(&mut self, name: String, value: Literal) {
        self.0.insert(name, value);
    }
    pub fn get(&self, token: Token) -> Result<Literal, RuntimeError> {
        let name = token.lexeme;
        if self.0.contains_key(&name) {
            Ok(self.0[&name].clone())
        } else {
            Err(RuntimeError::IdentifierError {
                line: token.line,
                msg: format!("Undefined variable '{name}'."),
            })
        }
    }
    pub fn assign(&mut self, token: Token, value: Literal) -> Result<(), RuntimeError> {
        let name = token.lexeme;
        match self.0.get_mut(&name) {
            Some(v) => {
                *v = value;
                Ok(())
            }
            None => Err(RuntimeError::UndefinedVariable {
                line: token.line,
                msg: format!("Trying to assign to an undefined variable '{name}'."),
            }),
        }
    }
}
