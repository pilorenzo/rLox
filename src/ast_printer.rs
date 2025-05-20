use crate::expression::Expr;

pub fn print_ast(expression: &Expr) -> String {
    accept(expression)
}

fn accept(expression: &Expr) -> String {
    match expression {
        Expr::Binary {
            left,
            operator,
            right,
        } => parentesize(&operator.lexeme, vec![left, right]),
        Expr::Grouping { expression } => parentesize("group", vec![expression]),
        Expr::Literal { value } => match value {
            // Some(lit) => format!("{:?}", lit),
            Some(lit) => format!("{}", lit),
            None => "nil".to_owned(),
        },
        Expr::Unary { operator, right } => parentesize(&operator.lexeme, vec![right]),
    }
}

fn parentesize(name: &str, exprs: Vec<&Expr>) -> String {
    let mut result = String::from("(");
    result.push_str(name);
    for expr in exprs {
        result.push(' ');
        result.push_str(&accept(expr));
    }
    result.push(')');

    result
}
