use crate::literal::Literal;
use std::fmt::Debug;
use std::fmt::Display;

#[derive(Debug)]
pub enum RuntimeError {
    Error { line: i32, msg: String },
    Return { value: Literal },
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::Error { line, msg } => {
                writeln!(f, "{} \n[line {}]", msg, line)
            }
            RuntimeError::Return { value } => {
                writeln!(f, "not an error, returning value {value}")
            }
        }
    }
}
