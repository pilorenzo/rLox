use crate::{environment::Environment, expression::Expr, statement::Stmt, Literal, Lox, TokenType};

pub enum RuntimeError {
    InvalidOperationError { line: i32, msg: String },
    IdentifierError { line: i32, msg: String },
    UndefinedVariable { line: i32, msg: String },
}

pub struct Interpreter;
impl Interpreter {
    pub fn interpret(lox: &mut Lox, statements: &Vec<Stmt>) {
        let mut env = Environment::global();
        for stmt in statements {
            match visit_statement(stmt, env) {
                Ok(e) => env = e,
                Err(error) => {
                    lox.runtime_error(error);
                    return;
                }
            }
        }
    }
}

fn visit_expression(expression: &Expr, env: &mut Environment) -> Result<Literal, RuntimeError> {
    let literal = match expression {
        Expr::Assignment { name, value } => {
            let value = visit_expression(value, env)?;
            env.assign(name.clone(), value.clone())?;
            value
        }
        Expr::Variable { name } => env.get(name.clone())?,
        Expr::Literal { value } => value.clone(),
        Expr::Grouping { expression } => visit_expression(expression, env)?,
        Expr::Unary { operator, right } => {
            let right = visit_expression(right, env)?;
            match operator.t_type {
                TokenType::Bang => Literal::Boolean(!is_truthy(right)),
                TokenType::Minus => Literal::Numeric(-to_num(operator.line, right)?),
                _ => Literal::Null,
            }
        }
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let (right, left) = (visit_expression(right, env)?, visit_expression(left, env)?);
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
    };
    Ok(literal)
}

fn is_truthy(value: Literal) -> bool {
    match value {
        Literal::Null => false,
        Literal::Boolean(b) => b,
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

fn visit_statement(stmt: &Stmt, mut env: Environment) -> Result<Environment, RuntimeError> {
    match stmt {
        Stmt::Expression { expression } => {
            visit_expression(expression, &mut env)?;
        }
        Stmt::Print { expression } => match visit_expression(expression, &mut env) {
            Ok(value) => println!("{value}"),
            Err(e) => return Err(e),
        },
        Stmt::Var { name, initializer } => {
            let val = if initializer.is_null() {
                Literal::Null
            } else {
                visit_expression(initializer, &mut env)?
            };
            env.define(name.lexeme.clone(), val);
        }
        Stmt::Block { statements } => {
            let mut inner_env = Environment::new(env);
            for statement in statements {
                inner_env = visit_statement(statement, inner_env)?;
            }
            env = *inner_env.outer.unwrap();
        }
    }
    Ok(env)
}
