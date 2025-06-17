use std::time::{SystemTime, UNIX_EPOCH};

use crate::environment::EnvironmentGraph;
use crate::{
    environment::Environment, expression::Expr, lox_callable::LoxCallable, statement::Stmt,
    Literal, Lox, TokenType,
};

pub enum RuntimeError {
    InvalidOperationError { line: i32, msg: String },
    IdentifierError { line: i32, msg: String },
    UndefinedVariable { line: i32, msg: String },
    Return { value: Literal },
}

pub struct Interpreter {
    pub graph: EnvironmentGraph,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Environment::global();
        define_globals(&mut globals);
        Self {
            graph: EnvironmentGraph::new(globals),
        }
    }

    pub fn get_last_environment_mut(&mut self) -> &mut Environment {
        self.graph
            .envs
            .last_mut()
            .expect("No environment found in the interpreter")
    }

    pub fn interpret(lox: &mut Lox, statements: &Vec<Stmt>) {
        let mut interpreter = Interpreter::new();
        let env = Environment::new();
        interpreter.graph.envs.push(env);
        for stmt in statements {
            match interpreter.visit_statement(stmt) {
                Ok(_) => continue,
                Err(error) => {
                    lox.runtime_error(error);
                    return;
                }
            }
        }
    }

    fn visit_statement(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
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
                self.get_last_environment_mut()
                    .define(name.lexeme.clone(), val);
            }
            Stmt::Block { statements } => {
                self.graph.envs.push(Environment::new());
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
                let function = LoxCallable::LoxFunction { declaration: dec };
                self.get_last_environment_mut().define(
                    declaration.name.lexeme.clone(),
                    Literal::Callable(Box::new(function)),
                )
            }
            Stmt::Return { keyword: _, value } => {
                let value = self.visit_expression(value)?;
                return Err(RuntimeError::Return { value });
            }
        }
        Ok(())
    }

    fn visit_expression(&mut self, expression: &Expr) -> Result<Literal, RuntimeError> {
        let literal = match expression {
            Expr::Assignment { name, value } => {
                let value = self.visit_expression(value)?;
                self.graph.assign(name.clone(), value.clone())?;
                value
            }
            Expr::Variable { name } => self.graph.get(name.clone())?,
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
        };
        Ok(literal)
    }

    pub fn execute_block(&mut self, statements: &Vec<Stmt>) -> Result<(), RuntimeError> {
        for statement in statements {
            self.visit_statement(statement)?;
        }
        Ok(())
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
        Literal::Callable(Box::new(LoxCallable::LoxAnonymous {
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
