use std::collections::HashMap;

use crate::{
    expression::Expr, interpreter::Interpreter, statement::Stmt, token_type::Token, Literal, Lox,
};

pub struct Resolver<'lox> {
    interpreter: Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    lox: &'lox mut Lox,
}

impl<'lox> Resolver<'lox> {
    fn new(interpreter: Interpreter, lox: &'lox mut Lox) -> Self {
        let scopes = Default::default();
        Resolver {
            interpreter,
            scopes,
            lox,
        }
    }

    fn visit_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block { statements } => self.visit_block(statements),
            Stmt::Var { name, initializer } => self.visit_var(name, initializer),
            _ => {}
        }
    }

    fn visit_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Variable { name } => self.visit_var_expression(expr, name),
            Expr::Literal { value } => self.visit_literal_expression(expr, value),
            Expr::Assignment { name, value } => self.visit_assignment_expression(expr, name, value),
            _ => {}
        }
    }

    fn visit_block(&mut self, statements: &[Stmt]) {
        self.begin_scope();
        for stmt in statements.iter() {
            self.visit_statement(stmt);
        }
        self.end_scope();
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn visit_var(&mut self, name: &Token, initializer: &Expr) {
        self.declare(name);
        if let Expr::Literal { value } = initializer {
            if *value != Literal::Null {
                self.visit_expression(initializer);
            }
        }
        self.define(name);
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), false);
        }
    }

    fn visit_var_expression(&mut self, var: &Expr, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(false) = scope.get_mut(&name.lexeme) {
                self.lox.error(
                    name.line,
                    "Can't read local variable in its own initializer.",
                )
            }
        }
        self.resolve_local(var, name);
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_local(&self, expr: &Expr, name: &Token) {
        let pos = self
            .scopes
            .iter()
            //.rev()
            .position(|s| s.contains_key(&name.lexeme));

        if let Some(i) = pos {
            self.interpreter.resolve(expr, i);
        }
    }

    fn visit_literal_expression(&self, expr: &Expr, value: &Literal) {
        todo!()
    }

    fn visit_assignment_expression(&mut self, expr: &Expr, name: &Token, value: &Expr) {
        self.visit_expression(value);
        self.resolve_local(expr, name);
    }
}
