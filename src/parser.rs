use crate::statement::Stmt;
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
        if self.match_type(vec![Var]) {
            self.var_declaration()
        } else {
            self.statement()
        }
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
        if self.match_type(vec![If]) {
            self.if_statement()
        } else if self.match_type(vec![Print]) {
            self.print_statement()
        } else if self.match_type(vec![LeftBrace]) {
            Ok(Stmt::Block {
                statements: self.block()?,
            })
        } else {
            self.expression_statement()
        }
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
        let expr = self.equality()?;
        if self.match_type(vec![Equal]) {
            let equals = self.previous();
            let value = Box::new(self.assignment()?);
            if let Expr::Variable { name } = expr {
                Ok(Expr::Assignment { name, value })
            } else {
                /* in lox there is no throw in this case, the error is only reported */
                Err(self.error(equals, "Invalid assignment target".to_owned()))
            }
        } else {
            Ok(expr)
        }
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

        println!("Equality expression {expr}");
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

        println!("comparison expression {expr}");
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

        println!("term expression {expr}");
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

        println!("factor expression {expr}");
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
            expr = self.primary()?;
        }

        println!("unary expression {expr}");
        Ok(expr)
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
