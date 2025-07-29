use crate::{
    literal::Literal, lox_callable::LoxCallable, runtime_error::RuntimeError, token_type::Token,
};
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::Display,
    rc::Rc,
    time::{SystemTime, UNIX_EPOCH},
};

/*
*   Instead of using environments with a reference to the enclosing one,
*   I preferred to use a Vec, which is easier to work with (and I believe a bit better for
*   performance too, since there is no need to iterate inside the get_at() and assign_at() function).
*   To limit the use of Rc<RefCell>>, that I believe are necessary to handle closures (at least for
*   how this interpreter is built), I have defined the EnvironmentType struct, which is probably
*   a terrible idea because:
*       - complicates the code;
*       - I'm not sure if it improves performance, because when I want to wrap the environment in a
*       Rc<RefCell> I need to pop the environment from the graph, wrap it and then push it.
*/

pub struct EnvironmentGraph {
    pub envs: Vec<EnvironmentType>,
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
            envs: vec![EnvironmentType::Standard { env: global }],
        }
    }

    pub fn assign(&mut self, token: &Token, value: Literal) -> Result<(), RuntimeError> {
        let name = &token.lexeme;
        for environment in self.envs.iter_mut().rev() {
            match environment {
                EnvironmentType::Standard { env } => {
                    if let Some(v) = env.dict.get_mut(name) {
                        *v = value;
                        return Ok(());
                    }
                }
                EnvironmentType::Closure { env } => {
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
        self.envs.push(EnvironmentType::Standard { env });
    }

    pub fn push_closure(&mut self, env: Environment) {
        let env = Rc::new(RefCell::new(env));
        self.envs.push(EnvironmentType::Closure { env });
    }

    pub fn pop(&mut self) -> Option<EnvironmentType> {
        self.envs.pop()
    }

    pub fn define(&mut self, name: &str, value: Literal) {
        match self
            .envs
            .last_mut()
            .expect("No environment found in the interpreter")
        {
            EnvironmentType::Standard { env } => env.define(name.to_owned(), value),
            EnvironmentType::Closure { env } => env.borrow_mut().define(name.to_owned(), value),
        }
    }

    pub fn change_last_to_closure(&mut self) -> Rc<RefCell<Environment>> {
        let env_node = self.pop().expect("No environment found in the interpreter");
        match env_node {
            EnvironmentType::Standard { env } => self.push_closure(env),
            EnvironmentType::Closure { env: _ } => self.envs.push(env_node),
        };

        if let Some(EnvironmentType::Closure { env }) = self.envs.last() {
            Rc::clone(env)
        } else {
            panic!("No environment found in the interpreter")
        }
    }

    pub fn get_at(&mut self, distance: usize, name: &Token) -> Result<Literal, RuntimeError> {
        let distance = self.envs.len() - 1 - distance;
        match self.envs.get(distance) {
            Some(env) => env.get_literal(name),
            None => Err(RuntimeError::Error {
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
            Err(RuntimeError::Error {
                line: t.line,
                msg: format!("Can't assign value {v} to variable {t} in selected scope {d}"),
            })
        };
        let distance = self.envs.len() - 1 - distance;

        match self.envs.get_mut(distance) {
            Some(env) => match env.assign_literal(token, value) {
                Some(()) => Ok(()),
                None => err(value, token, distance),
            },
            None => err(value, token, distance),
        }
    }
}

#[derive(Debug, Clone)]
pub enum EnvironmentType {
    Standard { env: Environment },
    Closure { env: Rc<RefCell<Environment>> },
}

impl EnvironmentType {
    pub fn get_literal(&self, name: &Token) -> Result<Literal, RuntimeError> {
        match self {
            EnvironmentType::Standard { env } => env.get_literal(name),
            EnvironmentType::Closure { env } => env.borrow().get_literal(name),
        }
    }

    pub fn assign_literal(&mut self, token: &Token, value: &Literal) -> Option<()> {
        let value = value.clone();
        match self {
            EnvironmentType::Standard { env } => {
                if let Some(v) = env.dict.get_mut(&token.lexeme) {
                    *v = value;
                    Some(())
                } else {
                    None
                }
            }
            EnvironmentType::Closure { env } => {
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
            EnvironmentType::Standard { env } => env.find_literal(name),
            EnvironmentType::Closure { env } => env.borrow().find_literal(name),
        }
    }
}

impl Display for EnvironmentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EnvironmentType::Standard { env } => writeln!(f, "{env}"),
            EnvironmentType::Closure { env } => writeln!(f, "{}", env.borrow()),
        }
    }
}

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

    pub fn get_literal(&self, name: &Token) -> Result<Literal, RuntimeError> {
        let value = self.dict.get(&name.lexeme);
        match value {
            Some(v) => Ok(v.clone()),
            None => Err(RuntimeError::Error {
                line: name.line,
                msg: format!("No entry found for key '{}'", name.lexeme),
            }),
        }
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
