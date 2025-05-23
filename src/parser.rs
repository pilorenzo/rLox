use crate::token_type::TokenType::*;
use crate::{Literal, Lox};
// {
//     Bang, BangEqual, EqualEqual, False, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus,
//     Nil, Number, Plus, RightParen, Slash, Star, String, True,
// };
use crate::{expression::Expr, token_type::Token, TokenType};

pub struct Parser<'a> {
    pub lox: &'a mut Lox,
    pub tokens: Vec<Token>,
    pub current: usize,
}

pub struct ParseError(pub String);

impl<'a> Parser<'a> {
    pub fn new(lox: &'a mut Lox, tokens: Vec<Token>) -> Self {
        Self {
            lox,
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
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
        } else if self.match_type(vec![LeftParen]) {
            let expr = self.expression()?;
            self.consume(RightParen, "Expect ')' after expression.".to_owned())?;
            Ok(Expr::Grouping {
                expression: Box::new(expr),
            })
        } else {
            Err(self.error(self.peek(), "Expected expression".to_owned()))
        }
    }

    fn consume(&mut self, token_type: TokenType, msg: String) -> Result<(), ParseError> {
        if self.check(token_type) {
            self.advance();
            return Ok(());
        }
        Err(self.error(self.peek(), msg))
    }

    fn error(&mut self, tok: Token, msg: String) -> ParseError {
        self.lox.token_error(&tok, &msg);
        /*
         *  In Lox it is returned a parse error here,
         *  which is a static class that extends RuntimeException
         */
        ParseError(msg)
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

    fn advance(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous();
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
