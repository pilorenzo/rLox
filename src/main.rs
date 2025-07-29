mod environment;
mod expression;
mod interpreter;
mod literal;
mod lox_callable;
mod parser;
mod resolver;
mod runtime_error;
mod scanner;
mod statement;
mod token_type;
use interpreter::Interpreter;
use parser::Parser;
use resolver::Resolver;
use runtime_error::RuntimeError;
use scanner::*;
use std::{
    env,
    fs::read,
    io::{self, Error, Write},
    str,
};
use token_type::Token;
use token_type::*;

fn main() -> Result<(), Error> {
    env::set_var("RUST_BACKTRACE", "1");
    let args: Vec<String> = env::args().collect();
    let mut lox = Lox::new();
    // println!("Hello, world!");
    // println!();
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

    /*
     *  This prompt runs all the code written in the previous iterations.
     *  I've added some code that makes the 'print' run only once, but this
     *  prevents some 'print' to show up if are defined inside a function and called later.
     *  Should rewrite run() to return the state of the lox program
     *  and interpret only the last line inserted.
     */
    fn run_prompt(&mut self) -> Result<(), Error> {
        println!("Write 'quit' to exit prompt.");
        let mut buffer = String::new();
        loop {
            let mut new_line = String::new();
            print!("> ");
            io::stdout().flush()?;
            if let Err(e) = io::stdin().read_line(&mut new_line) {
                eprintln!("Error reading line: {}", e)
            } else if new_line.trim_end() == "quit" {
                break;
            } else {
                let full_text = buffer.clone() + &new_line;
                self.run(&full_text);
                if !self.had_error && !self.had_runtime_error {
                    let lines: Vec<String> = new_line
                        .split(';')
                        .filter(|l| {
                            !(l.trim_start().starts_with("print")
                                || l.trim().is_empty()
                                || l.trim() == ";")
                        })
                        .map(|l| format!("{};", l))
                        .collect();
                    for l in lines {
                        buffer += &l;
                    }
                }
            }
        }
        Ok(())
    }

    fn run(&mut self, source: &str) {
        self.had_error = false;
        self.had_runtime_error = false;
        let mut scanner = Scanner::new(self, source);
        let tokens = scanner.scan_tokens().clone();
        let parse_result = Parser::new(self, tokens).parse();
        let Ok(statements) = parse_result else {
            let error_msg = parse_result.unwrap_err();
            eprintln!("ParseError [line: {}] {}", error_msg.0, error_msg.1);
            self.had_error = true;
            return;
        };
        // println!("Parsed");

        let mut interpreter = Interpreter::new();
        let mut resolver = Resolver::new(&mut interpreter, self);
        resolver.resolve(&statements);

        if self.had_error {
            return;
        }
        // println!("Resolved");

        for stmt in &statements {
            match interpreter.visit_statement(stmt) {
                Ok(_) => continue,
                Err(error) => {
                    self.runtime_error(error);
                    return;
                }
            }
        }
        // println!("Interpreted");
    }

    fn error(&mut self, line: i32, message: &str) {
        self.report(line, "", message);
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

    fn runtime_error(&mut self, error: RuntimeError) {
        eprintln!("{error}");
        self.had_runtime_error = true;
    }
}
