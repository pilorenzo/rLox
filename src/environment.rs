use crate::{interpreter::RuntimeError, token_type::Token, Literal};
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Environment {
    dict: HashMap<String, Literal>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            dict: HashMap::<String, Literal>::new(),
        }
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.dict.insert(name, value);
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result: String = String::default();
        for (name, value) in self.dict.iter() {
            result += &format!("{name}: {value}\n");
        }
        write!(f, "{result}")
    }
}

#[derive(Debug, Clone)]
pub enum EnvironmentNode {
    Standard { env: Environment },
    Closure { env: Rc<RefCell<Environment>> },
}

impl EnvironmentNode {
    // pub fn contains(&self, name: &str) -> bool {
    //     match self {
    //         EnvironmentNode::Standard { env } => env.dict.contains_key(name),
    //         EnvironmentNode::Closure { env } => env.borrow().dict.contains_key(name),
    //     }
    // }
    //
    pub fn get_literal(&self, name: &str) -> Literal {
        // println!("Searched variable {name}\n\n");
        // println!("Inside {}", self);
        match self {
            EnvironmentNode::Standard { env } => env.dict[name].clone(),
            EnvironmentNode::Closure { env } => env.borrow().dict[name].clone(),
        }
    }

    pub fn assign_literal(&mut self, token: &Token, value: &Literal) -> Option<()> {
        let value = value.clone();
        match self {
            EnvironmentNode::Standard { env } => {
                if let Some(v) = env.dict.get_mut(&token.lexeme) {
                    *v = value;
                    Some(())
                } else {
                    None
                }
            }
            EnvironmentNode::Closure { env } => {
                if let Some(v) = env.borrow_mut().dict.get_mut(&token.lexeme) {
                    *v = value;
                    Some(())
                } else {
                    None
                }
            }
        }
    }
}

impl Display for EnvironmentNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnvironmentNode::Standard { env } => writeln!(f, "{env}"),
            EnvironmentNode::Closure { env } => writeln!(f, "{}", env.borrow()),
        }
    }
}

pub struct EnvironmentGraph {
    pub envs: Vec<EnvironmentNode>,
}

impl EnvironmentGraph {
    pub fn new(global: Environment) -> Self {
        Self {
            envs: vec![EnvironmentNode::Standard { env: global }],
        }
    }

    // pub fn get(&self, token: &Token) -> Result<Literal, RuntimeError> {
    //     let name = &token.lexeme;
    //     for env in self.envs.iter().rev() {
    //         if env.contains(name) {
    //             return Ok(env.get_literal(name));
    //         }
    //     }
    //
    //     Err(RuntimeError::IdentifierError {
    //         line: token.line,
    //         msg: format!("Undefined variable '{name}'."),
    //     })
    // }
    //
    // pub fn assign(&mut self, token: Token, value: Literal) -> Result<(), RuntimeError> {
    //     let name = &token.lexeme;
    //     for environment in self.envs.iter_mut().rev() {
    //         match environment {
    //             EnvironmentNode::Standard { env } => {
    //                 if let Some(v) = env.dict.get_mut(name) {
    //                     *v = value;
    //                     return Ok(());
    //                 }
    //             }
    //             EnvironmentNode::Closure { env } => {
    //                 if let Some(v) = env.borrow_mut().dict.get_mut(name) {
    //                     *v = value;
    //                     return Ok(());
    //                 }
    //             }
    //         };
    //     }
    //
    //     Err(RuntimeError::UndefinedVariable {
    //         line: token.line,
    //         msg: format!("Trying to assign to an undefined variable '{name}'."),
    //     })
    // }
    //
    pub fn push(&mut self, env: Environment) {
        self.envs.push(EnvironmentNode::Standard { env })
    }

    pub fn push_closure(&mut self, env: Environment) {
        let env = Rc::new(RefCell::new(env));
        self.envs.push(EnvironmentNode::Closure { env })
    }

    pub fn pop(&mut self) -> Option<EnvironmentNode> {
        self.envs.pop()
    }

    pub fn define(&mut self, name: String, value: Literal) {
        match self
            .envs
            .last_mut()
            .expect("No environment found in the interpreter")
        {
            EnvironmentNode::Standard { env } => env.define(name, value),
            EnvironmentNode::Closure { env } => env.borrow_mut().define(name, value),
        }
    }

    pub fn change_last_to_closure(&mut self) -> Rc<RefCell<Environment>> {
        let env_node = self.pop().expect("No environment found in the interpreter");
        match env_node {
            EnvironmentNode::Standard { env } => self.push_closure(env),
            EnvironmentNode::Closure { env: _ } => self.envs.push(env_node),
        };

        if let Some(EnvironmentNode::Closure { env }) = self.envs.last() {
            Rc::clone(env)
        } else {
            panic!("No environment found in the interpreter")
        }
    }

    pub fn get_at(&mut self, distance: &usize, name: &Token) -> Result<Literal, RuntimeError> {
        match self.envs.get(*distance) {
            Some(env) => Ok(env.get_literal(&name.lexeme)),
            None => Err(RuntimeError::UndefinedVariable {
                line: name.line,
                msg: format!("Can't get variable {name} in selected scope {distance}"),
            }),
        }
    }

    pub fn assign_at(
        &mut self,
        distance: &usize,
        token: &Token,
        value: &Literal,
    ) -> Result<(), RuntimeError> {
        let err = |v, t: &Token, d| {
            Err(RuntimeError::UndefinedVariable {
                line: t.line,
                msg: format!("Can't assign value {v} to variable {t} in selected scope {d}"),
            })
        };
        match self.envs.get_mut(*distance) {
            Some(env) => match env.assign_literal(token, value) {
                Some(()) => Ok(()),
                None => err(value, token, distance),
            },
            None => err(value, token, distance),
        }
    }
}
