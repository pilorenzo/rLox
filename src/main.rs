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
    env::set_var("RUST_BACKTRACE", "1");
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    println!("Hello, world!");
    println!();
    match args.len() {
        3.. => Err(Error::new(io::ErrorKind::Other, "Usage: rlox <<script>>")),
        2 => lox.run_file(args.get(1).unwrap()),
        _ => lox.run_prompt(),
    }?;
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
        let parse_result = Parser::new(self, tokens).parse();
        let Ok(statements) = parse_result else {
            let error_msg = parse_result.unwrap_err();
            println!("ParseError [line: {}] {}", error_msg.0, error_msg.1);
            self.had_error = true;
            return;
        };

        Lox::resolve_and_interpret(self, &statements);
    }

    fn resolve_and_interpret(lox: &mut Lox, statements: &[Stmt]) {
        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter, lox);
        // for stmt in statements {
        //     resolver.visit_statement(stmt);
        // }
        resolver.resolve(statements);

        if lox.had_error {
            return;
        }

        println!("Ended resolve\n");
        // println!("Interpreter {interpreter}");

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
