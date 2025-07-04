use std::{collections::HashMap, fmt::Display};

use crate::{
    expression::Expr,
    interpreter::Interpreter,
    statement::{FunctionDeclaration, Stmt},
    token_type::Token,
    Literal, Lox,
};

#[derive(Clone, Copy)]
pub enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Clone, Copy)]
pub enum ClassType {
    None,
    Class,
    SubClass,
}

pub struct Resolver<'lox, 'int> {
    lox: &'lox mut Lox,
    interpreter: &'int mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl<'lox, 'int> Display for Resolver<'lox, 'int> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.scopes)
    }
}

impl<'lox, 'int> Resolver<'lox, 'int> {
    pub fn new(interpreter: &'int mut Interpreter, lox: &'lox mut Lox) -> Self {
        Resolver {
            lox,
            interpreter,
            scopes: Default::default(),
            current_function: FunctionType::None,
            current_class: ClassType::None,
        }
    }

    pub fn visit_statement(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block { statements } => self.visit_block(statements),
            Stmt::Var { name, initializer } => self.visit_var(name, initializer),
            Stmt::Fun { declaration } => self.visit_func(declaration),
            Stmt::Class {
                name,
                methods,
                superclass,
            } => self.visit_class(name, methods, superclass),
            Stmt::Expression { expression } => self.visit_expr_statement(expression),
            Stmt::Print { expression } => self.visit_print(expression),
            Stmt::Return { keyword, value } => self.visit_return(keyword, value),
            Stmt::While { condition, body } => self.visit_while(condition, body),
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => self.visit_if(condition, then_stmt, else_stmt),
        }
    }

    fn visit_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Variable { name } => self.visit_var_expression(expr, name),
            Expr::Literal { value: _ } => self.visit_literal_expression(),
            Expr::Assignment { name, value } => self.visit_assignment_expression(expr, name, value),
            Expr::Grouping { expression } => self.visit_grouping_expression(expression),
            Expr::Unary { operator: _, right } => self.visit_unary_expression(right),
            Expr::This { keyword } => self.visit_this_expression(expr, keyword),
            Expr::Super { keyword, method: _ } => self.visit_super_expression(expr, keyword),
            Expr::Get { object, name: _ } => self.visit_get_expression(object),
            Expr::Set {
                object,
                name: _,
                value,
            } => self.visit_set_expression(object, value),
            Expr::Logical {
                left,
                operator: _,
                right,
            } => self.visit_logical_expression(left, right),
            Expr::Call {
                callee,
                paren: _,
                args,
            } => self.visit_call_expression(callee, args),
            Expr::Binary {
                left,
                operator: _,
                right,
            } => self.visit_binary_expression(left, right),
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
            let old_value = scope.insert(name.lexeme.clone(), false);
            if old_value.is_some() {
                let lexeme = &name.lexeme;
                self.lox.error(
                    name.line,
                    &format!("Already a variable named '{lexeme}' in this scope"),
                );
            }
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
        // println!("Resolving {}", self);
    }

    fn resolve_local(&mut self, expr: &Expr, name: &Token) {
        let pos = self
            .scopes
            .iter()
            .rev()
            .position(|s| s.contains_key(&name.lexeme));

        // let tok_name = &name.lexeme;
        // println!("Position {pos:?} of {tok_name}");
        // println!("Token value {}: line:{}", &name.literal, &name.line);
        // println!("Scopes {:?}", self.scopes);

        if let Some(i) = pos {
            /* Index is different from book, because here global is environment 0 */
            let index = self.scopes.len() - i;
            self.interpreter.resolve(expr, index);
        }
    }

    fn visit_assignment_expression(&mut self, expr: &Expr, name: &Token, value: &Expr) {
        self.visit_expression(value);
        self.resolve_local(expr, name);
    }

    fn visit_func(&mut self, declaration: &FunctionDeclaration) {
        self.declare(&declaration.name);
        self.define(&declaration.name);
        self.resolve_function(declaration, FunctionType::Function);
    }

    fn resolve_function(&mut self, declaration: &FunctionDeclaration, func_type: FunctionType) {
        let enclosing_func = self.current_function;
        self.current_function = func_type;
        // self.begin_scope();
        for param in declaration.params.iter() {
            self.declare(param);
            self.define(param);
        }
        self.visit_block(&declaration.body);
        // self.end_scope();
        self.current_function = enclosing_func;
    }

    fn visit_literal_expression(&self) {}

    fn visit_expr_statement(&mut self, expression: &Expr) {
        self.visit_expression(expression);
    }

    fn visit_print(&mut self, expression: &Expr) {
        self.visit_expression(expression);
    }

    fn visit_return(&mut self, keyword: &Token, value: &Expr) {
        if let FunctionType::None = self.current_function {
            let line = keyword.line;
            self.lox.error(line, "Can't return from top-level code.");
        }
        match value {
            Expr::Literal {
                value: Literal::Null,
            } => {}
            _ => {
                if let FunctionType::Initializer = self.current_function {
                    let message = "Can't return value from initializer";
                    self.lox.error(keyword.line, message);
                }
                self.visit_expression(value);
            }
        }
    }

    fn visit_while(&mut self, condition: &Expr, body: &Stmt) {
        self.visit_expression(condition);
        self.visit_statement(body);
    }

    fn visit_if(&mut self, condition: &Expr, then_stmt: &Stmt, else_stmt: &Option<Box<Stmt>>) {
        self.visit_expression(condition);
        self.visit_statement(then_stmt);
        if let Some(else_branch) = else_stmt {
            self.visit_statement(else_branch)
        }
    }

    fn visit_grouping_expression(&mut self, expression: &Expr) {
        self.visit_expression(expression);
    }

    fn visit_unary_expression(&mut self, right: &Expr) {
        self.visit_expression(right);
    }

    fn visit_logical_expression(&mut self, left: &Expr, right: &Expr) {
        self.visit_expression(left);
        self.visit_expression(right);
    }

    fn visit_call_expression(&mut self, callee: &Expr, args: &[Expr]) {
        self.visit_expression(callee);
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_binary_expression(&mut self, left: &Expr, right: &Expr) {
        self.visit_expression(left);
        self.visit_expression(right);
    }

    fn visit_class(
        &mut self,
        name: &Token,
        methods: &[FunctionDeclaration],
        superclass: &Option<Box<Expr>>,
    ) {
        let enclosing_class = self.current_class;
        self.current_class = ClassType::Class;

        self.declare(name);
        self.define(name);

        if let Some(sup_class) = superclass {
            if let Expr::Variable { name: sup_name } = &(**sup_class) {
                if sup_name.lexeme == name.lexeme {
                    let message = "A class can't inherit from itself";
                    self.lox.error(sup_name.line, message);
                }
            }

            self.current_class = ClassType::SubClass;

            self.visit_expression(sup_class);

            self.begin_scope();
            self.scopes
                .last_mut()
                .unwrap()
                .insert("super".to_owned(), true);
        }

        self.begin_scope();
        self.scopes
            .last_mut()
            .unwrap()
            .insert("this".to_owned(), true);

        for method in methods {
            let func_type = if method.name.lexeme == "init" {
                FunctionType::Initializer
            } else {
                FunctionType::Method
            };
            self.resolve_function(method, func_type);
        }
        self.end_scope();

        if superclass.is_some() {
            self.end_scope();
        }

        self.current_class = enclosing_class;
    }

    fn visit_get_expression(&mut self, object: &Expr) {
        self.visit_expression(object);
    }

    fn visit_set_expression(&mut self, object: &Expr, value: &Expr) {
        self.visit_expression(value);
        self.visit_expression(object);
    }

    fn visit_this_expression(&mut self, expr: &Expr, keyword: &Token) {
        if let ClassType::None = self.current_class {
            self.lox
                .error(keyword.line, "Can't use this outside of a class")
        } else {
            self.resolve_local(expr, keyword)
        }
    }

    fn visit_super_expression(&mut self, expr: &Expr, keyword: &Token) {
        match self.current_class {
            ClassType::None => {
                let msg = "Can't use 'super' outside of a class";
                self.lox.error(keyword.line, msg);
            }
            ClassType::Class => {
                let msg = "Can't use 'super' in a class without superclass";
                self.lox.error(keyword.line, msg);
            }
            ClassType::SubClass => {}
        }
        self.resolve_local(expr, keyword);
    }
}
