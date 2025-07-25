use core::fmt;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::ptr::{self};
use std::rc::Rc;

use crate::environment::EnvironmentNode;
use crate::interpreter::Interpreter;
use crate::runtime_error::RuntimeError;
use crate::token_type::Token;
use crate::{environment::Environment, statement::FunctionDeclaration, Literal};

#[derive(Debug, Clone, PartialEq)]
pub struct LoxFunction {
    declaration: FunctionDeclaration,
    closures: Vec<Rc<RefCell<Environment>>>,
    is_initializer: bool,
}
impl LoxFunction {
    pub fn new(
        declaration: FunctionDeclaration,
        closures: Vec<Rc<RefCell<Environment>>>,
        is_initializer: bool,
    ) -> Self {
        Self {
            declaration,
            closures,
            is_initializer,
        }
    }

    fn is_in_graph(interpreter: &Interpreter, closure: &Rc<RefCell<Environment>>) -> bool {
        for environment in interpreter.graph.envs.iter() {
            if let EnvironmentNode::Closure { env } = environment {
                if ptr::eq(&*env.borrow(), &*closure.borrow()) {
                    return true;
                }
            }
        }
        false
    }

    pub fn bind(&self, instance: Rc<RefCell<LoxInstance>>) -> Self {
        let mut environment = Environment::new();
        environment.define("this".to_owned(), Literal::Class(instance));
        let closure = Rc::new(RefCell::new(environment));

        let mut closures = vec![];
        if self.closures[0].borrow().find_literal("super").is_some() {
            closures.push(Rc::clone(&self.closures[0]));
            closures.push(closure);
            for clos in self.closures.iter().skip(1) {
                closures.push(Rc::clone(clos));
            }
        } else {
            closures.push(closure);
            for clos in &self.closures {
                closures.push(Rc::clone(clos));
            }
        }

        Self {
            declaration: self.declaration.clone(),
            closures,
            is_initializer: self.is_initializer,
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, RuntimeError> {
        // println!("function name {}", self.declaration.name);
        // println!("function arity {}", self.declaration.params.len());
        // println!("function args {:?}", arguments);
        //
        // println!("Interpreter before {interpreter}");
        let mut closure_count = 0usize;
        for clos in self.closures.iter() {
            if !LoxFunction::is_in_graph(interpreter, clos) {
                interpreter.graph.envs.push(EnvironmentNode::Closure {
                    env: Rc::clone(clos),
                });
                closure_count += 1;
            }
        }
        // println!("Interpreter after {interpreter}");

        interpreter.graph.push(Environment::new());
        for (param, arg) in self.declaration.params.iter().zip(arguments.iter()) {
            // println!("param {param}");
            // println!("arg {arg}");
            interpreter.graph.define(&param.lexeme, arg.clone())
        }

        let block_res = interpreter.execute_block(&self.declaration.body);
        let res = match block_res {
            Err(RuntimeError::Return { value }) => {
                if self.is_initializer {
                    let this = Token::new_this(self.declaration.name.line);
                    self.closures[0].borrow().get_literal(&this)
                } else {
                    Ok(value)
                }
            }
            Err(e) => Err(e),
            Ok(()) => {
                if self.is_initializer {
                    let this = Token::new_this(self.declaration.name.line);
                    self.closures[0].borrow().get_literal(&this)
                } else {
                    Ok(Literal::Null)
                }
            }
        };
        interpreter.graph.pop();
        for _ in 0..closure_count {
            interpreter.graph.pop();
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
    superclass: Option<Box<LoxClass>>,
    methods: HashMap<String, LoxFunction>,
}

impl LoxClass {
    pub fn new(
        name: String,
        superclass: Option<Box<LoxClass>>,
        methods: HashMap<String, LoxFunction>,
    ) -> Self {
        Self {
            name: ClassName(name),
            superclass,
            methods,
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, RuntimeError> {
        let instance = Rc::new(RefCell::new(LoxInstance::new(self.clone())));
        if let Some(init) = self.find_method("init") {
            init.bind(Rc::clone(&instance))
                .call(interpreter, arguments)?;
        }
        Ok(Literal::Class(instance))
    }

    pub fn find_method(&self, lexeme: &str) -> Option<&LoxFunction> {
        let class_method = self.methods.get(lexeme);
        match (class_method, &self.superclass) {
            (None, Some(sup_class)) => sup_class.find_method(lexeme),
            _ => class_method,
        }
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
            LoxCallable::Class { class } => class.call(interpreter, arguments),
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

#[derive(Debug, PartialEq)]
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

    pub fn get(instance: Rc<RefCell<LoxInstance>>, name: &Token) -> Result<Literal, RuntimeError> {
        match instance.borrow().fields.get(&name.lexeme) {
            Some(l) => Ok(l.clone()),

            None => match instance.borrow().class.find_method(&name.lexeme) {
                Some(function) => {
                    let function = function.bind(Rc::clone(&instance));
                    Ok(Literal::new_function(function))
                }

                _ => Err(RuntimeError::Error {
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
