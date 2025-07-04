use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::environment::EnvironmentGraph;
use crate::lox_callable::{LoxClass, LoxFunction, LoxInstance};
use crate::token_type::Token;
use crate::{
    environment::Environment, expression::Expr, lox_callable::LoxCallable, statement::Stmt,
    Literal, TokenType,
};

#[derive(Debug)]
pub enum RuntimeError {
    InvalidOperationError { line: i32, msg: String },
    // IdentifierError { line: i32, msg: String },
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
        let mut global = Environment::new();
        define_globals(&mut global);
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
                Ok(value) => println!("{value}"),
                Err(e) => return Err(e),
            },
            Stmt::Var { name, initializer } => {
                let val = if initializer.is_null() {
                    Literal::Null
                } else {
                    self.visit_expression(initializer)?
                };
                self.graph.define(name.lexeme.clone(), val);
            }
            Stmt::Block { statements } => {
                self.graph.push(Environment::new());
                self.execute_block(statements)?;
                self.graph.envs.pop();
            }
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => {
                if is_truthy(&self.visit_expression(condition)?) {
                    self.visit_statement(then_stmt)?
                } else if let Some(branch) = else_stmt {
                    self.visit_statement(branch)?
                }
            }
            Stmt::While { condition, body } => {
                while is_truthy(&self.visit_expression(condition)?) {
                    self.visit_statement(body)?
                }
            }
            Stmt::Fun { declaration } => {
                let dec = (*declaration).clone();
                let closure = self.graph.change_last_to_closure();
                let function = LoxFunction::new(dec, closure, false);
                let function = LoxCallable::Function { function };
                self.graph.define(
                    declaration.name.lexeme.clone(),
                    Literal::Callable(Box::new(function)),
                )
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
                // let mut sup_class = None;
                let mut super_lox_class = None;
                if let Some(sc) = superclass {
                    let sup_class = Some(self.visit_expression(sc)?);
                    let Expr::Variable { name } = &(**sc) else {
                        panic!("superclass is not a variable");
                    };
                    let throw = || {
                        Err(RuntimeError::InvalidOperationError {
                            line: name.line,
                            msg: format!("Superclass of {name} must be a class"),
                        })
                    };
                    match sup_class {
                        Some(Literal::Callable(c)) => match *c {
                            LoxCallable::Class { class: cl } => {
                                super_lox_class = Some(Box::new(cl))
                            }
                            _ => return throw(),
                        },
                        _ => return throw(),
                    }
                }
                self.graph.define(name.lexeme.clone(), Literal::Null);

                if let Some(ref class) = super_lox_class {
                    self.graph.push(Environment::new());
                    let class = Box::new(LoxCallable::Class {
                        class: *class.clone(),
                    });
                    self.graph
                        .define("super".to_owned(), Literal::Callable(class));
                }

                let mut runtime_methods: HashMap<_, _> = Default::default();
                let closure = self.graph.change_last_to_closure();
                for method in methods {
                    let name = method.name.lexeme.clone();
                    let func = LoxFunction::new(
                        method.clone(),
                        Rc::clone(&closure),
                        method.name.lexeme == "init",
                    );
                    runtime_methods.insert(name, func);
                }

                let class = LoxCallable::Class {
                    class: LoxClass::new(name.lexeme.clone(), super_lox_class, runtime_methods),
                };
                let class = Box::new(class);

                if superclass.is_some() {
                    self.graph.pop();
                }
                self.graph.assign(name.clone(), Literal::Callable(class))?;
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expr) -> Result<Literal, RuntimeError> {
        let literal = match expression {
            Expr::Assignment { name, value } => {
                let value = self.visit_expression(value)?;
                // self.graph.assign(name.clone(), value.clone())?;
                // value
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
                match self.locals.get(expression) {
                    Some(distance) => self.graph.get_at(distance, name)?,
                    None => self.graph.envs[0].get_literal(&name.lexeme),
                }
            }
            Expr::Literal { value } => value.clone(),
            Expr::Grouping { expression } => self.visit_expression(expression)?,
            Expr::Unary { operator, right } => {
                let right = self.visit_expression(right)?;
                match operator.t_type {
                    TokenType::Bang => Literal::Boolean(!is_truthy(&right)),
                    TokenType::Minus => Literal::Numeric(-to_num(operator.line, right)?),
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
                    TokenType::Greater => Literal::Boolean(to_num(l, left)? > to_num(l, right)?),
                    TokenType::GreaterEqual => {
                        Literal::Boolean(to_num(l, left)? >= to_num(l, right)?)
                    }
                    TokenType::Less => Literal::Boolean(to_num(l, left)? < to_num(l, right)?),
                    TokenType::LessEqual => Literal::Boolean(to_num(l, left)? <= to_num(l, right)?),

                    TokenType::EqualEqual => Literal::Boolean(left == right),
                    TokenType::BangEqual => Literal::Boolean(left != right),

                    TokenType::Minus => Literal::Numeric(to_num(l, left)? - to_num(l, right)?),
                    TokenType::Slash => Literal::Numeric(to_num(l, left)? / to_num(l, right)?),
                    TokenType::Star => Literal::Numeric(to_num(l, left)? * to_num(l, right)?),
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
                        if is_truthy(&left) {
                            result = Some(left);
                        }
                    }
                    _ => {
                        if !is_truthy(&left) {
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
                    LoxInstance::get(self, instance, name)? // instance.get(name)?
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
                Some(distance) => self.graph.get_at(distance, keyword)?,
                None => self.graph.envs[0].get_literal(&keyword.lexeme),
            },
            Expr::Super { keyword, method } => {
                let distance = self
                    .locals
                    .get(expression)
                    .expect("super expression not found in interpreter locals");

                let superclass_literal = self.graph.get_at(distance, keyword)?;
                let mut superclass = None;
                if let Literal::Callable(boxed_class) = superclass_literal {
                    if let LoxCallable::Class { class } = *boxed_class {
                        superclass = Some(class);
                    }
                }
                let false_token =
                    Token::new(TokenType::Identifier, "this", Literal::Null, keyword.line);
                let distance = &(distance + 1usize);
                let instance = self.graph.get_at(distance, &false_token)?;
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
                Literal::Callable(Box::new(LoxCallable::Function { function }))
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

fn is_truthy(value: &Literal) -> bool {
    match value {
        Literal::Null => false,
        Literal::Boolean(b) => *b,
        _ => true,
    }
}

fn to_num(line: i32, lit: Literal) -> Result<f64, RuntimeError> {
    match lit {
        Literal::Numeric(n) => Ok(n),
        _ => Err(RuntimeError::InvalidOperationError {
            line,
            msg: format!("Can't cast {lit} to numeric"),
        }),
    }
}

fn define_globals(env: &mut Environment) {
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
    )
}
