use crate::{ast_printer, expression::Expr, LiteralType, TokenType};

struct InvalidOperationError(String);

fn visit(expression: &Expr) -> Result<LiteralType, InvalidOperationError> {
    let literal = match expression {
        Expr::Literal { value } => value.clone(),
        Expr::Grouping { expression } => visit(expression)?,
        Expr::Unary { operator, right } => {
            let right = visit(right)?;
            match operator.t_type {
                TokenType::Bang => LiteralType::Boolean(!is_truthy(right)),
                TokenType::Minus => LiteralType::Numeric(-to_num(right)?),
                _ => LiteralType::Null,
            }
        }
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            let (right, left) = (visit(right)?, visit(left)?);
            match operator.t_type {
                TokenType::Greater => LiteralType::Boolean(to_num(left)? > to_num(right)?),
                TokenType::GreaterEqual => LiteralType::Boolean(to_num(left)? >= to_num(right)?),
                TokenType::Less => LiteralType::Boolean(to_num(left)? < to_num(right)?),
                TokenType::LessEqual => LiteralType::Boolean(to_num(left)? <= to_num(right)?),

                TokenType::EqualEqual => LiteralType::Boolean(left == right),
                TokenType::BangEqual => LiteralType::Boolean(left != right),

                TokenType::Minus => LiteralType::Numeric(to_num(left)? - to_num(right)?),
                TokenType::Slash => LiteralType::Numeric(to_num(left)? / to_num(right)?),
                TokenType::Star => LiteralType::Numeric(to_num(left)? * to_num(right)?),
                TokenType::Plus => match (&left, &right) {
                    (LiteralType::Numeric(l), LiteralType::Numeric(r)) => {
                        LiteralType::Numeric(l + r)
                    }
                    (LiteralType::Letters(l), LiteralType::Letters(r)) => {
                        LiteralType::Letters(format!("{l}{r}"))
                    }
                    _ => {
                        return Err(InvalidOperationError(format!(
                            "Can't add {right} to {left}"
                        )))
                    }
                }, // LiteralType::Numeric(to_num(left)? * to_num(right)?),
                _ => LiteralType::Null,
            }
        }
    };
    Ok(literal)
}

fn is_truthy(value: LiteralType) -> bool {
    match value {
        LiteralType::Null => false,
        LiteralType::Boolean(b) => b,
        _ => true,
    }
}

fn to_num(lit: LiteralType) -> Result<f64, InvalidOperationError> {
    match lit {
        LiteralType::Numeric(n) => Ok(n),
        _ => Err(InvalidOperationError(format!(
            "Can't cast {lit} to numeric"
        ))),
    }
}
