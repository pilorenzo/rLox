use crate::statement::{FunctionDeclaration, Stmt};
use crate::token_type::TokenType::*;
use crate::{expression::Expr, token_type::Token, TokenType};
use crate::{Literal, Lox};

pub struct Parser<'a> {
    pub lox: &'a mut Lox,
    pub tokens: Vec<Token>,
    pub current: usize,
}

pub struct ParseError(pub Token, pub String);

impl<'a> Parser<'a> {
    pub fn new(lox: &'a mut Lox, tokens: Vec<Token>) -> Self {
        Self {
            lox,
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut statements = Vec::<Stmt>::new();
        while !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        Ok(statements)
        // self.expression();
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        if self.match_type(vec![Class]) {
            self.class_declaration()
        } else if self.match_type(vec![Fun]) {
            self.function("function")
        } else if self.match_type(vec![Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(Identifier, "Expect class name")?;

        let mut superclass = None;
        if self.match_type(vec![Less]) {
            self.consume(Identifier, "Expect superclass name")?;
            superclass = Some(Box::new(Expr::Variable {
                name: self.previous(),
            }))
        }

        self.consume(LeftBrace, "Expect '{' before class body")?;
        let mut methods = vec![];
        while !self.check(RightBrace) && !self.is_at_end() {
            let stmt = self.function("method")?;
            if let Stmt::Fun { declaration } = stmt {
                methods.push(declaration);
            } else {
                panic!("function() has not returned a function statement: {stmt}");
            }
        }
        self.consume(RightBrace, "Expect '}' after class body")?;
        Ok(Stmt::Class {
            name,
            methods,
            superclass,
        })
    }

    fn function(&mut self, kind: &str) -> Result<Stmt, ParseError> {
        let name = self.consume(Identifier, &format!("Expect {kind} name"))?;
        self.consume(LeftParen, &format!("Expect '(' after {kind} name"))?;
        let mut params = vec![];
        if !self.check(RightParen) {
            loop {
                if params.len() >= 16 {
                    self.lox
                        .error(self.peek().line, "Can't have more than 16 parameters");
                }
                params.push(self.consume(Identifier, "Expect parameter name.")?);
                if !self.match_type(vec![Comma]) {
                    break;
                }
            }
        }
        self.consume(RightParen, "Expect ')' after parameters")?;
        self.consume(LeftBrace, &format!("Expect '{{' before {kind} body"))?;
        let body = self.block()?;
        Ok(Stmt::Fun {
            declaration: FunctionDeclaration { name, params, body },
        })
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = self.consume(Identifier, "Expect variable name")?;
        let initializer = if self.match_type(vec![Equal]) {
            self.expression()?
        } else {
            Expr::Literal {
                value: Literal::Null,
            }
        };

        self.consume(Semicolon, "Expect ';' after variable declaration.")?;

        let initializer = Box::new(initializer);
        Ok(Stmt::Var { name, initializer })
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        if self.match_type(vec![For]) {
            self.for_statement()
        } else if self.match_type(vec![If]) {
            self.if_statement()
        } else if self.match_type(vec![Return]) {
            self.return_statement()
        } else if self.match_type(vec![Print]) {
            self.print_statement()
        } else if self.match_type(vec![While]) {
            self.while_statement()
        } else if self.match_type(vec![LeftBrace]) {
            Ok(Stmt::Block {
                statements: self.block()?,
            })
        } else {
            self.expression_statement()
        }
    }

    fn for_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.match_type(vec![Semicolon]) {
            Option::None
        } else if self.match_type(vec![Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(Semicolon) {
            self.expression()?
        } else {
            /* if no condition, loops continuously */
            Expr::Literal {
                value: Literal::Boolean(true),
            }
        };

        self.consume(Semicolon, "Expect ';' after loop condition")?;

        let increment = if !self.check(RightParen) {
            Some(self.expression()?)
        } else {
            Option::None
        };

        self.consume(RightParen, "Expect ')' after for clauses")?;

        let mut body = self.statement()?;

        if let Some(expr) = increment {
            body = Stmt::Block {
                statements: vec![
                    body,
                    Stmt::Expression {
                        expression: Box::new(expr),
                    },
                ],
            };
        }

        body = Stmt::While {
            condition: Box::new(condition),
            body: Box::new(body),
        };

        if let Some(stmt) = initializer {
            body = Stmt::Block {
                statements: vec![stmt, body],
            };
        }

        Ok(body)
    }

    fn if_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(LeftParen, "Expect '(' after 'if'.")?;
        let condition = Box::new(self.expression()?);
        self.consume(RightParen, "Expect ')' after if condition.")?;
        let then_stmt = Box::new(self.statement()?);
        let else_stmt = if self.match_type(vec![Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_stmt,
            else_stmt,
        })
    }

    fn while_statement(&mut self) -> Result<Stmt, ParseError> {
        self.consume(LeftParen, "Expect '(' after 'while'.")?;
        let condition = Box::new(self.expression()?);
        self.consume(RightParen, "Expect ')' after while condition.")?;
        let body = Box::new(self.statement()?);
        Ok(Stmt::While { condition, body })
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts: Vec<Stmt> = vec![];
        while !self.check(RightBrace) && !self.is_at_end() {
            stmts.push(self.declaration()?);
        }
        self.consume(RightBrace, "Expect '}' after block.")?;
        Ok(stmts)
    }

    fn print_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::Print {
            expression: Box::new(expr),
        })
    }

    fn return_statement(&mut self) -> Result<Stmt, ParseError> {
        let keyword = self.previous();
        let value = if !self.check(Semicolon) {
            self.expression()?
        } else {
            Expr::Literal {
                value: Literal::Null,
            }
        };
        let value = Box::new(value);
        self.consume(Semicolon, "Expect ';' after return.")?;
        Ok(Stmt::Return { keyword, value })
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.expression()?;
        self.consume(Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::Expression {
            expression: Box::new(expr),
        })
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        // let expr = self.equality()?;
        let expr = self.or()?;

        if self.match_type(vec![Equal]) {
            let equals = self.previous();
            let value = Box::new(self.assignment()?);
            if let Expr::Variable { name } = expr {
                Ok(Expr::Assignment { name, value })
            } else if let Expr::Get { object, name } = expr {
                Ok(Expr::Set {
                    object,
                    name,
                    value,
                })
            } else {
                /* in lox there is no throw in this case, the error is only reported */
                Err(self.error(equals, "Invalid assignment target".to_owned()))
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.and()?;
        while self.match_type(vec![Or]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.equality()?;
        while self.match_type(vec![And]) {
            let operator = self.previous();
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.comparison()?;
        while self.match_type(vec![BangEqual, EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        // println!("Equality expression {expr}");
        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.term()?;
        while self.match_type(vec![Greater, GreaterEqual, Less, LessEqual]) {
            let operator = self.previous();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        // println!("comparison expression {expr}");
        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.factor()?;
        while self.match_type(vec![Minus, Plus]) {
            let operator = self.previous();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        // println!("term expression {expr}");
        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.unary()?;
        while self.match_type(vec![Slash, Star]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        // println!("factor expression {expr}");
        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        let expr;
        if self.match_type(vec![Bang, Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            expr = Expr::Unary {
                operator,
                right: Box::new(right),
            };
        } else {
            expr = self.call()?;
        }

        // println!("unary expression {expr}");
        Ok(expr)
    }

    fn call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.primary()?;
        loop {
            if self.match_type(vec![LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_type(vec![Dot]) {
                let name = self.consume(Identifier, "Expect property name after '.'.")?;
                let object = Box::new(expr);
                expr = Expr::Get { object, name };
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParseError> {
        let mut args = Vec::<Expr>::new();
        if !self.check(RightParen) {
            loop {
                args.push(self.expression()?);
                if args.len() > 16 {
                    let line = self.peek().line;
                    self.lox.error(line, "Can't have more than 16 arguments");
                }
                if !self.match_type(vec![Comma]) {
                    break;
                }
            }
        }

        let callee = Box::new(callee);
        let paren = self.consume(RightParen, "Expect ')' after arguments")?;
        Ok(Expr::Call {
            callee,
            paren,
            args,
        })
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        if self.match_type(vec![False]) {
            Ok(Expr::Literal {
                value: Literal::Boolean(false),
            })
        } else if self.match_type(vec![True]) {
            Ok(Expr::Literal {
                value: Literal::Boolean(true),
            })
        } else if self.match_type(vec![Nil]) {
            Ok(Expr::Literal {
                value: Token::empty_literal(),
            })
        } else if self.match_type(vec![Number, Chars]) {
            Ok(Expr::Literal {
                value: self.previous().literal,
            })
        } else if self.match_type(vec![This]) {
            Ok(Expr::This {
                keyword: self.previous(),
            })
        } else if self.match_type(vec![Identifier]) {
            Ok(Expr::Variable {
                name: self.previous(),
            })
        } else if self.match_type(vec![LeftParen]) {
            let expr = self.expression()?;
            self.consume(RightParen, "Expect ')' after expression.")?;
            Ok(Expr::Grouping {
                expression: Box::new(expr),
            })
        } else {
            Err(self.error(self.peek(), "Expected expression".to_owned()))
        }
    }

    fn consume(&mut self, token_type: TokenType, msg: &str) -> Result<Token, ParseError> {
        if self.check(token_type) {
            // self.advance();
            Ok(self.advance())
        } else {
            Err(self.error(self.peek(), msg.to_owned()))
        }
    }

    fn error(&mut self, tok: Token, msg: String) -> ParseError {
        self.lox.token_error(&tok, &msg);
        /*
         *  In Lox it is returned a parse error here,
         *  which is a static class that extends RuntimeException
         */
        ParseError(tok, msg)
    }

    fn match_type(&mut self, tokens: Vec<TokenType>) -> bool {
        for token_type in tokens {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().t_type == token_type
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().t_type == TokenType::Eof
    }

    fn peek(&self) -> Token {
        self.tokens
            .get(self.current)
            .expect("token not found in peek")
            .clone()
        // Token::new(tok.t_type, &tok.lexeme, tok.literal, tok.line)
    }

    fn previous(&self) -> Token {
        self.tokens
            .get(self.current - 1)
            .expect("token not found in peek")
            .clone()
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().t_type == Semicolon {
                return;
            }
            let should_return = matches!(
                self.peek().t_type,
                Class | Fun | Var | For | If | While | Print | Return
            );

            if should_return {
                return;
            }

            self.advance();
        }
    }
}
