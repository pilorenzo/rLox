use crate::{ast_printer, expression::Expr, Literal, Lox, TokenType};

pub struct InvalidOperationError {
    pub line: i32,
    pub msg: String,
}

pub struct Interpreter;
impl Interpreter {
    pub fn interpret(lox: &mut Lox, expr: &Expr) {
        // let value = visit(expr)?;
        match visit(expr) {
            Ok(value) => println!("{value}"),
            Err(error) => lox.runtime_error(error),
        }
    }
}

fn visit(expression: &Expr) -> Result<Literal, InvalidOperationError> {
    let literal = match expression {
        Expr::Literal { value } => value.clone(),
        Expr::Grouping { expression } => visit(expression)?,
        Expr::Unary { operator, right } => {
            let right = visit(right)?;
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
            let (right, left) = (visit(right)?, visit(left)?);
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
                        return Err(InvalidOperationError {
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

fn to_num(line: i32, lit: Literal) -> Result<f64, InvalidOperationError> {
    match lit {
        Literal::Numeric(n) => Ok(n),
        _ => Err(InvalidOperationError {
            line,
            msg: format!("Can't cast {lit} to numeric"),
        }),
    }
}
