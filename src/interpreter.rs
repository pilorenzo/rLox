use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::environment::{EnvironmentGraph, EnvironmentNode};
use crate::lox_callable::{LoxClass, LoxFunction, LoxInstance};
use crate::token_type::Token;
use crate::{
    environment::Environment, expression::Expr, lox_callable::LoxCallable, statement::Stmt,
    Literal, TokenType,
};

#[derive(Debug)]
pub enum RuntimeError {
    InvalidOperationError { line: i32, msg: String },
    UndefinedVariable { line: i32, msg: String },
    PropertyError { line: i32, msg: String },
    Return { value: Literal },
}

pub struct Interpreter {
    pub graph: EnvironmentGraph,
    locals: HashMap<Expr, usize>,
}

impl Display for Interpreter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::default();
        for (i, e) in self.graph.envs.iter().enumerate() {
            result += &format!("Environment {i}:\n{e}");
        }
        result += "-----------------\n";
        for (k, v) in self.locals.iter() {
            result += &format!("Locals {k}: {v}\n");
        }
        write!(f, "{result}")
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let global = Environment::global();
        Self {
            graph: EnvironmentGraph::new(global),
            locals: Default::default(),
        }
    }

    pub fn visit_statement(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expression { expression } => {
                self.visit_expression(expression)?;
            }
            Stmt::Print { expression } => match self.visit_expression(expression) {
                Ok(value) => println!(">>> {value}"),
                Err(e) => return Err(e),
            },
            Stmt::Var { name, initializer } => {
                let val = if initializer.is_null() {
                    Literal::Null
                } else {
                    self.visit_expression(initializer)?
                };
                self.graph.define(&name.lexeme, val);
            }
            Stmt::Block { statements } => {
                self.graph.push(Environment::new());
                self.execute_block(statements)?;
                self.graph.pop();
            }
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                if self.visit_expression(condition)?.is_truthy() {
                    self.visit_statement(then_stmt)?
                } else if let Some(branch) = else_stmt {
                    self.visit_statement(branch)?
                }
            }
            Stmt::While { condition, body } => {
                while self.visit_expression(condition)?.is_truthy() {
                    self.visit_statement(body)?
                }
            }
            Stmt::Fun { declaration } => {
                // println!("Declaration of {}", declaration.name.lexeme);
                // println!("Interpreter {}", self);
                let dec = (*declaration).clone();
                let closure = self.graph.change_last_to_closure();
                let this_env = self
                    .graph
                    .envs
                    .iter()
                    .rev()
                    .find(|e| e.find_literal("this").is_some());
                // .first();
                // let this_env = self.locals.get(&this_expr).map(|d| &self.graph.envs[*d]);
                // println!("Environment of this {:?}", this_env);
                let closure_vec = if let Some(EnvironmentNode::Closure { env }) = this_env {
                    vec![Rc::clone(env), closure]
                } else {
                    vec![closure]
                };

                let function = LoxFunction::new(dec, closure_vec, false);
                let name = &declaration.name.lexeme;
                self.graph.define(name, Literal::new_function(function));
            }
            Stmt::Return { keyword: _, value } => {
                let value = self.visit_expression(value)?;
                return Err(RuntimeError::Return { value });
            }
            Stmt::Class {
                name,
                methods,
                superclass,
            } => {
                let mut super_lox_class = None;
                if let Some(sc) = superclass {
                    super_lox_class = self.visit_expression(sc)?.get_class();
                    if super_lox_class.is_none() {
                        return Err(RuntimeError::InvalidOperationError {
                            line: name.line,
                            msg: format!("Superclass of {} must be a class", name.lexeme),
                        });
                    }
                }
                self.graph.define(&name.lexeme, Literal::Null);

                // println!("Class definition");
                // println!("Interpreter \n{}", self);
                let size = self.graph.envs.len();
                println!("Size: {size}");
                if let Some(ref sup_class) = super_lox_class {
                    self.graph.push(Environment::new());
                    println!("pushing super env");
                    let sp = Literal::new_class(sup_class.clone());
                    self.graph.define("super", sp);
                    // print!("Interpreter \n{}", self);
                }

                let mut runtime_methods: HashMap<_, _> = Default::default();
                let closure = self.graph.change_last_to_closure();
                for method in methods {
                    let name = method.name.lexeme.clone();
                    let func = LoxFunction::new(
                        method.clone(),
                        vec![Rc::clone(&closure)],
                        method.name.lexeme == "init",
                    );
                    runtime_methods.insert(name, func);
                }

                let class = LoxClass::new(
                    name.lexeme.clone(),
                    super_lox_class.map(Box::new),
                    runtime_methods,
                );

                if superclass.is_some() {
                    // while self.graph.envs.len() > size {
                    println!("popping env");
                    self.graph.pop();
                    // }
                    // print!("Interpreter \n{}", self);
                }
                self.graph.assign(name, Literal::new_class(class))?;
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expr) -> Result<Literal, RuntimeError> {
        let literal = match expression {
            Expr::Assignment { name, value } => {
                let value = self.visit_expression(value)?;
                match self.locals.get(expression) {
                    Some(distance) => self.graph.assign_at(distance, name, &value)?,
                    None => self.graph.envs[0].assign_literal(name, &value).unwrap(),
                }
                value
            }
            Expr::Variable { name } => {
                // print!("Interpreter \n{}", self);
                // println!("---------------------------");
                // println!("Expression {expression}");
                //
                println!("Line {}", &name.line);
                match self.locals.get(expression) {
                    Some(distance) => self.graph.get_at(*distance, name)?,
                    None => self.graph.envs[0].get_literal(&name.lexeme),
                }
            }
            Expr::Literal { value } => value.clone(),
            Expr::Grouping { expression } => self.visit_expression(expression)?,
            Expr::Unary { operator, right } => {
                let right = self.visit_expression(right)?;
                match operator.t_type {
                    TokenType::Bang => Literal::Boolean(!right.is_truthy()),
                    TokenType::Minus => Literal::Numeric(-right.to_num(operator.line)?),
                    _ => Literal::Null,
                }
            }
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let (right, left) = (self.visit_expression(right)?, self.visit_expression(left)?);
                let l = operator.line;
                match operator.t_type {
                    TokenType::Greater => Literal::Boolean(left.to_num(l)? > right.to_num(l)?),
                    TokenType::GreaterEqual => {
                        Literal::Boolean(left.to_num(l)? >= right.to_num(l)?)
                    }
                    TokenType::Less => Literal::Boolean(left.to_num(l)? < right.to_num(l)?),
                    TokenType::LessEqual => Literal::Boolean(left.to_num(l)? <= right.to_num(l)?),

                    TokenType::EqualEqual => Literal::Boolean(left == right),
                    TokenType::BangEqual => Literal::Boolean(left != right),

                    TokenType::Minus => Literal::Numeric(left.to_num(l)? - right.to_num(l)?),
                    TokenType::Slash => Literal::Numeric(left.to_num(l)? / right.to_num(l)?),
                    TokenType::Star => Literal::Numeric(left.to_num(l)? * right.to_num(l)?),
                    TokenType::Plus => match (&left, &right) {
                        (Literal::Numeric(l), Literal::Numeric(r)) => Literal::Numeric(l + r),
                        (Literal::Letters(l), Literal::Letters(r)) => {
                            Literal::Letters(format!("{l}{r}"))
                        }
                        _ => {
                            return Err(RuntimeError::InvalidOperationError {
                                line: l,
                                msg: format!("Can't add {right} to {left}"),
                            })
                        }
                    },
                    _ => Literal::Null,
                }
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.visit_expression(left)?;
                let mut result = None;
                match operator.t_type {
                    TokenType::Or => {
                        if left.is_truthy() {
                            result = Some(left);
                        }
                    }
                    _ => {
                        if !left.is_truthy() {
                            result = Some(left);
                        }
                    }
                };
                match result {
                    Some(left) => left,
                    None => self.visit_expression(right)?,
                }
            }
            Expr::Call {
                callee,
                paren,
                args,
            } => {
                let callee = self.visit_expression(callee)?;
                let mut arguments = Vec::<Literal>::new();
                for arg in args {
                    arguments.push(self.visit_expression(arg)?);
                }
                let Literal::Callable(function) = callee else {
                    return Err(RuntimeError::InvalidOperationError {
                        line: paren.line,
                        msg: "Can only call function and classes".to_owned(),
                    });
                };
                if arguments.len() != function.get_arity() {
                    return Err(RuntimeError::InvalidOperationError {
                        line: paren.line,
                        msg: format!(
                            "Expected {} arguments, got {}",
                            function.get_arity(),
                            arguments.len()
                        ),
                    });
                }
                function.call(self, arguments)?
            }
            Expr::Get { object, name } => {
                // println!("Get Expr visited");
                let object = self.visit_expression(object)?;
                if let Literal::Class(instance) = object {
                    LoxInstance::get(self, instance, name)?
                } else {
                    return Err(RuntimeError::PropertyError {
                        line: name.line,
                        msg: "Only instances have properties".to_owned(),
                    });
                }
            }
            Expr::Set {
                object,
                name,
                value,
            } => {
                // println!("Set Expr visited");
                let object = self.visit_expression(object)?;
                if let Literal::Class(instance) = object {
                    let value = self.visit_expression(value)?;
                    instance.borrow_mut().set(name, &value);
                    value
                } else {
                    return Err(RuntimeError::PropertyError {
                        line: name.line,
                        msg: "Only instances have fields".to_owned(),
                    });
                }
            }
            Expr::This { keyword } => match self.locals.get(expression) {
                Some(distance) => self.graph.get_at(*distance, keyword)?,
                None => self.graph.envs[0].get_literal(&keyword.lexeme),
            },
            Expr::Super { keyword, method } => {
                let distance = self
                    .locals
                    .get(expression)
                    .expect("super expression not found in interpreter locals");

                let superclass_literal = self.graph.get_at(*distance, keyword)?;
                let mut superclass = None;
                if let Literal::Callable(boxed_class) = superclass_literal {
                    if let LoxCallable::Class { class } = *boxed_class {
                        superclass = Some(class);
                    }
                }
                let token = Token::new(TokenType::Identifier, "this", Literal::Null, keyword.line);
                let distance = &(distance + 1usize);
                let instance = self.graph.get_at(*distance, &token)?;
                let Literal::Class(instance) = instance else {
                    panic!("'this' not found at distance {distance}");
                };

                let Some(superclass) = superclass else {
                    panic!("superclass not found in line {}", keyword.line);
                };

                let func = superclass.find_method(&method.lexeme);

                let Some(function) = func else {
                    return Err(RuntimeError::PropertyError {
                        line: method.line,
                        msg: format!("undefined property '{}'.", method.lexeme),
                    });
                };
                let function = function.bind(self, instance);
                Literal::new_function(function)
            }
        };
        Ok(literal)
    }

    pub fn execute_block(&mut self, statements: &Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.visit_statement(statement)?;
        }
        Ok(())
    }

    pub fn resolve(&mut self, expr: &Expr, i: usize) {
        self.locals.insert(expr.clone(), i);
    }
}
