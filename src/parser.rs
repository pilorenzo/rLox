use crate::token_type::TokenType::*;
use crate::LiteralType;
// {
//     Bang, BangEqual, EqualEqual, False, Greater, GreaterEqual, LeftParen, Less, LessEqual, Minus,
//     Nil, Number, Plus, RightParen, Slash, Star, String, True,
// };
use crate::{expression::Expr, token_type::Token, TokenType};

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }

    fn expression(&mut self) -> Expr {
        self.equality()
    }

    fn equality(&mut self) -> Expr {
        let mut expr = self.comparison();
        while self.match_type(vec![BangEqual, EqualEqual]) {
            let operator = self.previous();
            let right = self.comparison();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        expr
    }

    fn comparison(&mut self) -> Expr {
        let mut expr = self.term();
        while self.match_type(vec![Greater, GreaterEqual, Less, LessEqual]) {
            let operator = self.previous();
            let right = self.term();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        expr
    }

    fn term(&mut self) -> Expr {
        let mut expr = self.factor();
        while self.match_type(vec![Minus, Plus]) {
            let operator = self.previous();
            let right = self.factor();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        expr
    }

    fn factor(&mut self) -> Expr {
        let mut expr = self.unary();
        while self.match_type(vec![Slash, Star]) {
            let operator = self.previous();
            let right = self.unary();
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        expr
    }

    fn unary(&mut self) -> Expr {
        let expr;
        if self.match_type(vec![Bang, Minus]) {
            let operator = self.previous();
            let right = self.unary();
            expr = Expr::Unary {
                operator,
                right: Box::new(right),
            }
        } else {
            expr = self.primary();
        }

        expr
    }

    fn primary(&mut self) -> Expr {
        if self.match_type(vec![False]) {
            Expr::Literal {
                value: Some(LiteralType::Boolean(false)),
            }
        } else if self.match_type(vec![True]) {
            Expr::Literal {
                value: Some(LiteralType::Boolean(true)),
            }
        } else if self.match_type(vec![Nil]) {
            Expr::Literal {
                value: Some(Token::empty_literal()),
            }
        } else if self.match_type(vec![Number, String]) {
            Expr::Literal {
                value: Some(self.previous().literal),
            }
        } else
        // if self.match_type(vec![LeftParen]) {
        {
            let expr = self.expression();
            // self.consume
            Expr::Grouping {
                expression: Box::new(expr),
            }
        }
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
}
