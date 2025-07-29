use crate::{literal::Literal, Lox, Token, TokenType};
use std::char;

pub struct Scanner<'a> {
    lox: &'a mut Lox,
    source: &'a str,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: i32,
}

impl<'a> Scanner<'a> {
    pub fn new(lox: &'a mut Lox, source: &'a str) -> Self {
        Self {
            lox,
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> &Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        let eof_token = Token::new(TokenType::Eof, "", Literal::Null, self.line);
        self.tokens.push(eof_token);

        &self.tokens
    }

    fn scan_token(&mut self) {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            '-' => self.add_token(TokenType::Minus),
            '+' => self.add_token(TokenType::Plus),
            ';' => self.add_token(TokenType::Semicolon),
            '*' => self.add_token(TokenType::Star),
            '!' => self.add_token_conditional('=', TokenType::BangEqual, TokenType::Bang),
            '=' => self.add_token_conditional('=', TokenType::EqualEqual, TokenType::Equal),
            '<' => self.add_token_conditional('=', TokenType::LessEqual, TokenType::Less),
            '>' => self.add_token_conditional('=', TokenType::GreaterEqual, TokenType::Greater),
            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                self.line += 1;
            }
            '"' => self.scan_string(),
            _ if c.is_numeric() => self.scan_number(),
            _ if Scanner::is_alphabetic(c) => self.scan_identifier(),
            _ => self.lox.error(self.line, "Unexpected character"),
        }
    }

    fn current_char(&self) -> char {
        self.source
            .chars()
            .nth(self.current)
            .expect("Char not found")
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.current_char() != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.current_char()
        }
    }

    fn peek_next(&self) -> char {
        let next = self.current + 1;
        self.source.chars().nth(next).unwrap_or('\0')
    }

    fn advance(&mut self) -> char {
        let c = self.current_char();
        self.current += 1;
        c
    }

    fn add_token_conditional(&mut self, expected: char, t1: TokenType, t2: TokenType) {
        if self.match_char(expected) {
            self.add_token(t1)
        } else {
            self.add_token(t2)
        }
    }
    fn add_token(&mut self, t: TokenType) {
        self.add_token_with_literal(t, Literal::Null);
    }

    fn get_source_substring(&self) -> String {
        let mut char_by_indices = self.source.char_indices();
        let (start, _) = char_by_indices.nth(self.start).unwrap();
        let (current, _) = char_by_indices.nth(self.current - self.start - 1).unwrap();
        self.source[start..current].to_owned()
    }

    fn add_token_with_literal(&mut self, t: TokenType, literal: Literal) {
        let text = self.get_source_substring();
        self.tokens.push(Token::new(t, &text, literal, self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.chars().count()
    }

    fn scan_string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }
        if self.is_at_end() {
            return self.lox.error(self.line, "String not ended");
        }

        // the closing ".
        self.advance();

        // remove the "
        let substring = self.get_source_substring();
        let mut chars = substring.chars();
        chars.next();
        chars.next_back();

        let val = chars.as_str().to_owned();

        self.add_token_with_literal(TokenType::Chars, Literal::Letters(val));
    }

    fn scan_number(&mut self) {
        while self.peek().is_numeric() {
            self.advance();
        }

        // Look for a fractional part
        if self.peek() == '.' && self.peek_next().is_numeric() {
            self.advance();

            while self.peek().is_numeric() {
                self.advance();
            }
        }

        let text = self.get_source_substring();
        let literal = Literal::Numeric(text.parse::<f64>().unwrap());
        self.add_token_with_literal(TokenType::Number, literal)
    }

    fn is_alphabetic(c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_alphanumeric(c: char) -> bool {
        c.is_alphanumeric() || c == '_'
    }

    fn scan_identifier(&mut self) {
        while Scanner::is_alphanumeric(self.peek()) {
            self.advance();
        }

        let text = self.get_source_substring();
        let token_type = match Token::get_keyword_by_name(&text) {
            Some(t) => t,
            None => TokenType::Identifier,
        };
        self.add_token(token_type);
    }
}
