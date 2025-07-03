use core::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ptr::{self};
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
    is_initializer: bool,
}
impl LoxFunction {
    pub fn new(
        declaration: FunctionDeclaration,
        closure: Rc<RefCell<Environment>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            declaration,
            closure,
            is_initializer,
        }
    }

    pub fn is_closure_env_in_graph(&self, interpreter: &mut Interpreter) -> bool {
        let mut result = false;
        let outer = interpreter.graph.envs.last();
        if let Some(EnvironmentNode::Closure { env }) = outer {
            result = ptr::eq(&*env.borrow(), &*self.closure.borrow());
        }
        result
    }

    pub fn bind(&self, interpreter: &mut Interpreter, instance: Rc<RefCell<LoxInstance>>) -> Self {
        let mut environment = Environment::new();
        environment.define("this".to_owned(), Literal::Class(instance));
        let closure = Rc::new(RefCell::new(environment));
        /* is this necessary? */
        if !self.is_closure_env_in_graph(interpreter) {
            interpreter.graph.envs.push(EnvironmentNode::Closure {
                env: Rc::clone(&self.closure),
            });
        }
        /**/
        Self {
            declaration: self.declaration.clone(),
            closure,
            is_initializer: self.is_initializer,
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, RuntimeError> {
        let is_closure_in_graph = self.is_closure_env_in_graph(interpreter);
        if !is_closure_in_graph {
            interpreter.graph.envs.push(EnvironmentNode::Closure {
                env: Rc::clone(&self.closure),
            });
        }
        interpreter.graph.push(Environment::new());
        for (param, arg) in self.declaration.params.iter().zip(arguments.iter()) {
            interpreter.graph.define(param.lexeme.clone(), arg.clone())
        }
        // println!("\n\n###########################");
        // print!("Interpreter \n{interpreter}");
        // println!("---------------------------");
        // print!("Closure env :\n{}", self.closure.borrow());
        // println!("###########################\n\n");
        // println!("Function name {}", &self.declaration.name);
        // println!("Is initializer? {}", &self.is_initializer);
        let block_res = interpreter.execute_block(&self.declaration.body);
        // println!("Block result {block_res:?}");
        let res = match block_res {
            Err(RuntimeError::Return { value }) => {
                if self.is_initializer {
                    Ok(self.closure.borrow().get_literal("this"))
                } else {
                    Ok(value)
                }
            }
            Err(e) => Err(e),
            Ok(()) => {
                if self.is_initializer {
                    Ok(self.closure.borrow().get_literal("this"))
                } else {
                    Ok(Literal::Null)
                }
            }
        };
        interpreter.graph.envs.pop();
        if !is_closure_in_graph {
            interpreter.graph.envs.pop();
        }
        res
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
        class: LoxClass,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxClass {
    name: ClassName,
    methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(name: String, methods: HashMap<String, LoxFunction>) -> Self {
        Self {
            name: ClassName(name),
            methods,
        }
    }

    pub fn find_method(&self, lexeme: &str) -> Option<&LoxFunction> {
        self.methods.get(lexeme)
    }
}

impl Hash for LoxClass {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl LoxCallable {
    pub fn get_arity(&self) -> usize {
        match self {
            LoxCallable::Anonymous { arity, func: _ } => *arity,
            LoxCallable::Function { function } => function.declaration.params.len(),
            LoxCallable::Class { class } => {
                if let Some(function) = class.find_method("init") {
                    function.declaration.params.len()
                } else {
                    0
                }
            }
        }
    }

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, RuntimeError> {
        match self {
            LoxCallable::Anonymous { arity: _, func } => Ok((func)(arguments)),
            LoxCallable::Function { function } => function.call(interpreter, arguments),
            LoxCallable::Class { class } => {
                let instance = Rc::new(RefCell::new(LoxInstance::new(class.clone())));
                if let Some(initializer) = class.find_method("init") {
                    initializer
                        .bind(interpreter, Rc::clone(&instance))
                        .call(interpreter, arguments)?;
                }
                Ok(Literal::Class(instance))
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
            LoxCallable::Class { class } => write!(f, "class {}", class.name.0),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxInstance {
    class: LoxClass,
    fields: HashMap<String, Literal>,
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.class.name.0)
    }
}

impl LoxInstance {
    fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: Default::default(),
        }
    }

    pub fn get(
        interpreter: &mut Interpreter,
        instance: Rc<RefCell<LoxInstance>>,
        name: &Token,
    ) -> Result<Literal, RuntimeError> {
        match instance.borrow().fields.get(&name.lexeme) {
            Some(l) => Ok(l.clone()),

            None => match instance.borrow().class.find_method(&name.lexeme) {
                Some(function) => {
                    // println!("Adding function bind");
                    let function = function.bind(interpreter, Rc::clone(&instance));
                    // for closure in function.closures.iter() {
                    //     interpreter.graph.envs.push(EnvironmentNode::Closure {
                    //         env: Rc::clone(closure),
                    //     });
                    // }
                    Ok(Literal::Callable(Box::new(LoxCallable::Function {
                        function,
                    })))
                }

                /*
                 * Literal::Callable(Box::new(LoxCallable::Function {
                 *   function: m.clone(),
                 * }))
                 */
                _ => Err(RuntimeError::PropertyError {
                    line: name.line,
                    msg: format!("undefined property '{}'.", name.lexeme),
                }),
            },
        }
    }

    pub fn set(&mut self, name: &Token, value: &Literal) {
        self.fields.insert(name.lexeme.clone(), value.clone());
    }
}
