mod ast_printer;
mod environment;
mod expression;
mod interpreter;
mod lox_callable;
mod parser;
mod resolver;
mod scanner;
mod statement;
mod token_type;
use interpreter::{Interpreter, RuntimeError};
// use expression::Expr;
use parser::Parser;
use resolver::Resolver;
use scanner::*;
use statement::Stmt;
use std::{
    env,
    fs::read,
    io::{self, BufRead, Error, Write},
    str,
};
use token_type::Token;
use token_type::*;

fn main() -> Result<(), Error> {
    // env::set_var("RUST_BACKTRACE", "1");
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    println!("Hello, world!");
    println!();
    // let file = args.get(1).unwrap();
    // println!("file name {}", &file);
    match args.len() {
        3.. => Err(Error::new(io::ErrorKind::Other, "Usage: rlox <<script>>")),
        2 => lox.run_file(args.get(1).unwrap()),
        _ => lox.run_prompt(),
    }?;
    // Ok(())
    // let expression = Expr::Binary {
    //     left: Box::new(Expr::Unary {
    //         operator: Token::new(TokenType::Minus, "-", Token::empty_literal(), 1),
    //         right: Box::new(Expr::Literal {
    //             value: Some(LiteralType::Numeric(123.0)),
    //         }),
    //     }),
    //     operator: Token::new(TokenType::Star, "*", Token::empty_literal(), 1),
    //     right: Box::new(Expr::Grouping {
    //         expression: Box::new(Expr::Literal {
    //             value: Some(LiteralType::Numeric(99.0)),
    //         }),
    //     }),
    // };
    // println!("{}", ast_printer::print_ast(&expression));
    Ok(())
}

struct Lox {
    had_error: bool,
    had_runtime_error: bool,
}

impl Lox {
    fn new() -> Self {
        Self {
            had_error: false,
            had_runtime_error: false,
        }
    }

    fn run_file(&mut self, path: &str) -> Result<(), Error> {
        let bytes = read(path)?;
        self.run(str::from_utf8(&bytes).unwrap());
        if self.had_error {
            std::process::exit(65);
        } else if self.had_runtime_error {
            std::process::exit(70);
        } else {
            Ok(())
        }
    }

    fn run_prompt(&mut self) -> Result<(), Error> {
        for _ in io::stdin().lock().lines() {
            print!("> ");
            io::stdout().flush()?;

            match io::stdin().lock().lines().next() {
                Some(Ok(input)) => self.run(&input),
                Some(Err(e)) => eprintln!("Error reading line: {}", e),
                None => break,
            }
            self.had_error = false;
        }
        Ok(())
    }

    fn run(&mut self, source: &str) {
        let text = source.to_owned();
        let mut scanner = Scanner::new(self, text);
        let tokens = scanner.scan_tokens().clone();
        // // For now, just print the tokens.
        // println!("Read tokens");
        // for token in &tokens {
        //     println!("{token}");
        // }
        // println!("Read ended");
        let parse_result = Parser::new(self, tokens).parse();
        let Ok(statements) = parse_result else {
            // let error_msg = parse_result.unwrap_err().0;
            // println!("{error_msg}");
            self.had_error = true;
            return;
        };

        Lox::resolve_and_interpret(self, &statements);
        // println!("{}", ast_printer::print_ast(&expr));
        //
    }

    fn resolve_and_interpret(lox: &mut Lox, statements: &Vec<Stmt>) {
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter, lox);
        for stmt in statements {
            resolver.visit_statement(stmt);
        }

        if lox.had_error {
            return;
        }

        println!("Ended resolve\n");

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

    fn error(&mut self, line: i32, message: &str) {
        Self::report(self, line, "", message);
    }

    fn report(&mut self, line: i32, place: &str, message: &str) {
        eprintln!("[line {line}] Error {place}: {message}");
        self.had_error = true;
    }

    fn token_error(&mut self, tok: &Token, msg: &str) {
        if tok.t_type == TokenType::Eof {
            self.report(tok.line, "at end", msg);
        } else {
            let place = "at '".to_owned() + &tok.lexeme + "'";
            self.report(tok.line, &place, msg);
        }
    }

    pub fn runtime_error(&mut self, error: RuntimeError) {
        let (msg, line) = match error {
            RuntimeError::InvalidOperationError { line, msg } => (msg, line),
            // RuntimeError::IdentifierError { line, msg } => (msg, line),
            RuntimeError::UndefinedVariable { line, msg } => (msg, line),
            RuntimeError::PropertyError { line, msg } => (msg, line),
            RuntimeError::Return { value } => {
                (format!("not an error, returning value {value}"), -1)
            }
        };
        eprintln!("{} \n[line {}]", msg, line);
        self.had_runtime_error = true;
    }
}
