use std::cell::RefCell;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    environment::Environment, expression::Expr, lox_callable::LoxCallable, statement::Stmt,
    Literal, Lox, TokenType,
};

pub enum RuntimeError {
    InvalidOperationError { line: i32, msg: String },
    IdentifierError { line: i32, msg: String },
    UndefinedVariable { line: i32, msg: String },
}

pub struct Interpreter {
    pub globals: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Environment::global();
        define_globals(&globals);
        Self { globals }
    }

    pub fn interpret(lox: &mut Lox, statements: &Vec<Stmt>) {
        // let mut globals = Environment::global();
        // define_globals(&mut globals);
        let interpreter = Interpreter::new();
        let env = Rc::new(RefCell::new(Environment::new(interpreter.globals)));
        for stmt in statements {
            match visit_statement(stmt, Rc::clone(&env)) {
                Ok(_) => continue,
                Err(error) => {
                    lox.runtime_error(error);
                    return;
                }
            }
        }
    }
}

fn visit_expression(
    expression: &Expr,
    env: Rc<RefCell<Environment>>,
) -> Result<Literal, RuntimeError> {
    let literal = match expression {
        Expr::Assignment { name, value } => {
            let value = visit_expression(value, Rc::clone(&env))?;
            env.borrow_mut().assign(name.clone(), value.clone())?;
            value
        }
        Expr::Variable { name } => env.borrow_mut().get(name.clone())?,
        Expr::Literal { value } => value.clone(),
        Expr::Grouping { expression } => visit_expression(expression, env)?,
        Expr::Unary { operator, right } => {
            let right = visit_expression(right, env)?;
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
            let (right, left) = (
                visit_expression(right, Rc::clone(&env))?,
                visit_expression(left, Rc::clone(&env))?,
            );
            let l = operator.line;
            match operator.t_type {
                TokenType::Greater => Literal::Boolean(to_num(l, left)? > to_num(l, right)?),
                TokenType::GreaterEqual => Literal::Boolean(to_num(l, left)? >= to_num(l, right)?),
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
            let left = visit_expression(left, Rc::clone(&env))?;
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
                None => visit_expression(right, Rc::clone(&env))?,
            }
        }
        Expr::Call {
            callee,
            paren,
            args,
        } => {
            let callee = visit_expression(callee, Rc::clone(&env))?;
            let mut arguments = Vec::<Literal>::new();
            for arg in args {
                arguments.push(visit_expression(arg, Rc::clone(&env))?);
            }
            let Literal::Callable(function) = callee else {
                return Err(RuntimeError::InvalidOperationError {
                    line: paren.line,
                    msg: "Can only call function and classes".to_owned(),
                });
            };
            // let function = LoxCallable(f);
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
            function.call(Some(env), arguments)?
        }
    };
    Ok(literal)
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

fn visit_statement(
    stmt: &Stmt,
    mut env: Rc<RefCell<Environment>>,
) -> Result<Rc<RefCell<Environment>>, RuntimeError> {
    match stmt {
        Stmt::Expression { expression } => {
            visit_expression(expression, Rc::clone(&env))?;
        }
        Stmt::Print { expression } => match visit_expression(expression, Rc::clone(&env)) {
            Ok(value) => println!("{value}"),
            Err(e) => return Err(e),
        },
        Stmt::Var { name, initializer } => {
            let val = if initializer.is_null() {
                Literal::Null
            } else {
                visit_expression(initializer, Rc::clone(&env))?
            };
            env.borrow_mut().define(name.lexeme.clone(), val);
        }
        Stmt::Block { statements } => {
            let block_env = Rc::new(RefCell::new(Environment::new(Rc::clone(&env))));
            execute_block(statements, block_env)?;
        }
        Stmt::If {
            condition,
            then_stmt,
            else_stmt,
        } => {
            if is_truthy(&visit_expression(condition, Rc::clone(&env))?) {
                env = visit_statement(then_stmt, env)?
            } else if let Some(branch) = else_stmt {
                env = visit_statement(branch, Rc::clone(&env))?
            }
        }
        Stmt::While { condition, body } => {
            while is_truthy(&visit_expression(condition, Rc::clone(&env))?) {
                env = visit_statement(body, Rc::clone(&env))?
            }
        }
        Stmt::Fun { declaration } => {
            let dec = (*declaration).clone();
            let function = LoxCallable::LoxFunction { declaration: dec };
            env.borrow_mut().define(
                declaration.name.lexeme.clone(),
                Literal::Callable(Box::new(function)),
            )
        }
    }
    Ok(env)
}

pub fn execute_block(
    statements: &Vec<Stmt>,
    mut env: Rc<RefCell<Environment>>,
) -> Result<(), RuntimeError> {
    // let mut inner_env = &mut Environment::new(&mut env);
    for statement in statements {
        env = visit_statement(statement, env)?;
    }
    // env = *inner_env.outer.unwrap();
    // Ok(env)
    Ok(())
}

fn define_globals(env: &Rc<RefCell<Environment>>) {
    // let start = SystemTime::now();
    // let func = |_| {
    //     Literal::Numeric(
    //         SystemTime::now()
    //             .duration_since(UNIX_EPOCH)
    //             .expect("Time went backwards")
    //             .as_secs_f64(),
    //     )
    // };
    env.borrow_mut().define(
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
