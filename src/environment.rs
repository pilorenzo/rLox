use crate::{lox_callable::LoxCallable, runtime_error::RuntimeError, token_type::Token, Literal};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

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

    pub fn global() -> Self {
        let mut env = Environment::new();
        env.define(
            "clock".to_owned(),
            Literal::Callable(Box::new(LoxCallable::Anonymous {
                arity: 0,
                func: |_| {
                    Literal::Numeric(
                        SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .expect("Time went backwards")
                            .as_secs_f64(),
                    )
                },
            })),
        );
        env
    }

    pub fn define(&mut self, name: String, value: Literal) {
        self.dict.insert(name, value);
    }

    pub fn get_literal(&self, name: &str) -> Literal {
        self.dict
            .get(name)
            .unwrap_or_else(|| panic!("No entry found for key '{name}'"))
            .clone()
    }

    pub fn find_literal(&self, name: &str) -> Option<Literal> {
        self.dict.get(name).cloned()
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
    pub fn get_literal(&self, name: &str) -> Literal {
        match self {
            EnvironmentNode::Standard { env } => env.get_literal(name),
            EnvironmentNode::Closure { env } => env.borrow().get_literal(name),
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

    pub fn find_literal(&self, name: &str) -> Option<Literal> {
        match self {
            EnvironmentNode::Standard { env } => env.find_literal(name),
            EnvironmentNode::Closure { env } => env.borrow().find_literal(name),
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

impl Display for EnvironmentGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::default();
        for (i, e) in self.envs.iter().enumerate() {
            result += &format!("Environment {i}:\n{e}");
        }
        result += "-----------------\n";
        write!(f, "{result}")
    }
}

impl EnvironmentGraph {
    pub fn new(global: Environment) -> Self {
        Self {
            envs: vec![EnvironmentNode::Standard { env: global }],
        }
    }

    pub fn assign(&mut self, token: &Token, value: Literal) -> Result<(), RuntimeError> {
        let name = &token.lexeme;
        for environment in self.envs.iter_mut().rev() {
            match environment {
                EnvironmentNode::Standard { env } => {
                    if let Some(v) = env.dict.get_mut(name) {
                        *v = value;
                        return Ok(());
                    }
                }
                EnvironmentNode::Closure { env } => {
                    if let Some(v) = env.borrow_mut().dict.get_mut(name) {
                        *v = value;
                        return Ok(());
                    }
                }
            };
        }

        Err(RuntimeError::Error {
            line: token.line,
            msg: format!("Trying to assign to an undefined variable '{name}'."),
        })
    }

    pub fn push(&mut self, env: Environment) {
        self.envs.push(EnvironmentNode::Standard { env });
        // println!("EnvironmentGraph after push:\n{}", self);
    }

    pub fn push_closure(&mut self, env: Environment) {
        let env = Rc::new(RefCell::new(env));
        self.envs.push(EnvironmentNode::Closure { env });
        // println!("EnvironmentGraph after closure push:\n{}", self);
    }

    pub fn pop(&mut self) -> Option<EnvironmentNode> {
        self.envs.pop()
        // let opt = self.envs.pop();
        // println!("EnvironmentGraph after pop:\n{}", self);
        // opt
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        match self
            .envs
            .last_mut()
            .expect("No environment found in the interpreter")
        {
            EnvironmentNode::Standard { env } => env.define(name.to_owned(), value),
            EnvironmentNode::Closure { env } => env.borrow_mut().define(name.to_owned(), value),
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

    pub fn get_at(&mut self, distance: usize, name: &Token) -> Result<Literal, RuntimeError> {
        match self.envs.get(distance) {
            Some(env) => Ok(env.get_literal(&name.lexeme)),
            None => {
                // println!("\n\n###########################");
                // let mut result = String::default();
                // for (i, e) in self.envs.iter().enumerate() {
                //     result += &format!("Environment {i}:\n{e}");
                // }
                // println!("{result}");
                // println!("-------------------------------");
                // println!("distance {distance}, envs {}", self.envs.len());
                // println!("###########################\n\n");
                // panic!("Can't get variable {name} in selected scope {distance}");
                Err(RuntimeError::Error {
                    line: name.line,
                    msg: format!("Can't get variable {name} in selected scope {distance}"),
                })
            }
        }
    }

    pub fn assign_at(
        &mut self,
        distance: &usize,
        token: &Token,
        value: &Literal,
    ) -> Result<(), RuntimeError> {
        let err = |v, t: &Token, d| {
            Err(RuntimeError::Error {
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
