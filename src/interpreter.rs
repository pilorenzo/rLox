use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

use crate::environment::{EnvironmentGraph, EnvironmentNode};
use crate::lox_callable::{LoxClass, LoxFunction, LoxInstance};
use crate::runtime_error::RuntimeError;
use crate::statement::FunctionDeclaration;
use crate::token_type::Token;
use crate::{environment::Environment, expression::Expr, statement::Stmt, Literal, TokenType};

pub struct Interpreter {
    pub graph: EnvironmentGraph,
    locals: HashMap<Expr, usize>,
}

impl Display for Interpreter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = String::default();
        for (i, e) in self.graph.envs.iter().enumerate() {
            result += &format!("Environment {i}:\n{e}");
        }
        result += "-----------------\n";
        for (k, v) in self.locals.iter() {
            result += &format!("Locals {k}: {v}\n");
        }
        write!(f, "{result}")
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let global = Environment::global();
        Self {
            graph: EnvironmentGraph::new(global),
            locals: Default::default(),
        }
    }

    // fn print_locals(&self) -> String {
    //     let mut result = String::default();
    //     for (k, v) in self.locals.iter() {
    //         result += &format!("Locals {k}: {v}\n");
    //     }
    //     result
    // }

    pub fn visit_statement(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        match stmt {
            Stmt::Expression { expression } => self.visit_stmt_expression(expression),
            Stmt::Print { expression } => self.visit_stmt_print(expression),
            Stmt::Var { name, initializer } => self.visit_stmt_var(name, initializer),
            Stmt::Block { statements } => self.visit_stmt_block(statements),
            Stmt::If {
                condition,
                then_stmt,
                else_stmt,
            } => self.visit_stmt_if(condition, then_stmt, else_stmt),
            Stmt::While { condition, body } => self.visit_stmt_while(condition, body),
            Stmt::Fun { declaration } => self.visit_stmt_fun(declaration),
            Stmt::Return { keyword: _, value } => self.visit_stmt_return(value),
            Stmt::Class {
                name,
                methods,
                superclass,
            } => self.visit_stmt_class(name, methods, superclass),
        }
    }

    fn visit_expression(&mut self, expression: &Expr) -> Result<Literal, RuntimeError> {
        match expression {
            Expr::Literal { value } => Ok(value.clone()),
            Expr::Assignment { name, value } => self.visit_expr_assignment(expression, name, value),
            Expr::Variable { name } => self.visit_expr_variable(expression, name),
            Expr::Grouping { expression } => self.visit_expression(expression),
            Expr::Unary { operator, right } => self.visit_expr_unary(operator, right),
            Expr::Binary {
                left,
                operator,
                right,
            } => self.visit_expr_binary(operator, left, right),
            Expr::Logical {
                left,
                operator,
                right,
            } => self.visit_expr_logical(operator, left, right),
            Expr::Call {
                callee,
                paren,
                args,
            } => self.visit_expr_call(callee, paren, args),
            Expr::Get { object, name } => self.visit_expr_get(object, name),
            Expr::Set {
                object,
                name,
                value,
            } => self.visit_expr_set(object, name, value),
            Expr::This { keyword } => self.visit_expr_this(expression, keyword),
            Expr::Super { keyword, method } => self.visit_expr_super(expression, keyword, method),
        }
    }

    pub fn execute_block(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        for statement in statements {
            self.visit_statement(statement)?;
        }
        Ok(())
    }

    pub fn resolve(&mut self, expr: &Expr, i: usize) {
        self.locals.insert(expr.clone(), i);
    }

    fn visit_stmt_expression(&mut self, expression: &Expr) -> Result<(), RuntimeError> {
        self.visit_expression(expression)?;
        Ok(())
    }

    fn visit_stmt_print(&mut self, expression: &Expr) -> Result<(), RuntimeError> {
        match self.visit_expression(expression) {
            Ok(value) => Ok(println!(">>> {value}")),
            Err(e) => Err(e),
        }
    }

    fn visit_stmt_var(&mut self, name: &Token, initializer: &Expr) -> Result<(), RuntimeError> {
        let val = if initializer.is_null() {
            Literal::Null
        } else {
            self.visit_expression(initializer)?
        };
        self.graph.define(&name.lexeme, val);
        Ok(())
    }

    fn visit_stmt_block(&mut self, statements: &[Stmt]) -> Result<(), RuntimeError> {
        self.graph.push(Environment::new());
        println!("Pushed block");
        self.execute_block(statements)?;
        self.graph.pop();
        println!("Popped block");
        Ok(())
    }

    fn visit_stmt_if(
        &mut self,
        condition: &Expr,
        then_stmt: &Stmt,
        else_stmt: &Option<Box<Stmt>>,
    ) -> Result<(), RuntimeError> {
        if self.visit_expression(condition)?.is_truthy() {
            self.visit_statement(then_stmt)
        } else if let Some(branch) = else_stmt {
            self.visit_statement(branch)
        } else {
            Ok(())
        }
    }

    fn visit_stmt_while(&mut self, condition: &Expr, body: &Stmt) -> Result<(), RuntimeError> {
        while self.visit_expression(condition)?.is_truthy() {
            self.visit_statement(body)?
        }
        Ok(())
    }

    fn visit_stmt_fun(&mut self, declaration: &FunctionDeclaration) -> Result<(), RuntimeError> {
        let dec = (*declaration).clone();
        let closure = self.graph.change_last_to_closure();
        let this_env = self
            .graph
            .envs
            .iter()
            .rev()
            .find(|e| e.find_literal("this").is_some());
        let closure_vec = if let Some(EnvironmentNode::Closure { env }) = this_env {
            vec![Rc::clone(env), closure]
        } else {
            vec![closure]
        };

        let function = LoxFunction::new(dec, closure_vec, false);
        let name = &declaration.name.lexeme;
        self.graph.define(name, Literal::new_function(function));
        Ok(())
    }

    fn visit_stmt_return(&mut self, value: &Expr) -> Result<(), RuntimeError> {
        let value = self.visit_expression(value)?;
        Err(RuntimeError::Return { value })
    }

    fn visit_stmt_class(
        &mut self,
        name: &Token,
        methods: &[FunctionDeclaration],
        superclass: &Option<Box<Expr>>,
    ) -> Result<(), RuntimeError> {
        let mut super_lox_class = None;
        if let Some(sc) = superclass {
            super_lox_class = self.visit_expression(sc)?.get_class();
            if super_lox_class.is_none() {
                return Err(RuntimeError::Error {
                    line: name.line,
                    msg: format!("Superclass of {} must be a class", name.lexeme),
                });
            }
        }
        self.graph.define(&name.lexeme, Literal::Null);

        if let Some(ref sup_class) = super_lox_class {
            self.graph.push(Environment::new());
            let sp = Literal::new_class(sup_class.clone());
            self.graph.define("super", sp);
        }

        let mut runtime_methods: HashMap<_, _> = Default::default();
        let closure = self.graph.change_last_to_closure();
        for method in methods {
            let name = method.name.lexeme.clone();
            let func = LoxFunction::new(
                method.clone(),
                vec![Rc::clone(&closure)],
                method.name.lexeme == "init",
            );
            runtime_methods.insert(name, func);
        }

        let class = LoxClass::new(
            name.lexeme.clone(),
            super_lox_class.map(Box::new),
            runtime_methods,
        );

        if superclass.is_some() {
            self.graph.pop();
        }
        self.graph.assign(name, Literal::new_class(class))?;
        Ok(())
    }

    fn visit_expr_assignment(
        &mut self,
        expression: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Literal, RuntimeError> {
        let value = self.visit_expression(value)?;
        match self.locals.get(expression) {
            Some(distance) => self.graph.assign_at(distance, name, &value)?,
            None => self.graph.envs[0].assign_literal(name, &value).unwrap(),
        }
        Ok(value)
    }

    fn visit_expr_variable(
        &mut self,
        expression: &Expr,
        name: &Token,
    ) -> Result<Literal, RuntimeError> {
        // println!("Trying to get {}", name);
        // println!("Locals\n{}", self.print_locals());
        // println!("In expression {}", expression);
        // println!("Interpreter {}", self);
        match self.locals.get(expression) {
            Some(distance) => self.graph.get_at(*distance, name),
            None => self.graph.envs[0].get_literal(name),
        }
    }

    fn visit_expr_unary(
        &mut self,
        operator: &Token,
        right: &Expr,
    ) -> Result<Literal, RuntimeError> {
        let right = self.visit_expression(right)?;
        Ok(match operator.t_type {
            TokenType::Bang => Literal::Boolean(!right.is_truthy()),
            TokenType::Minus => Literal::Numeric(-right.to_num(operator.line)?),
            _ => Literal::Null,
        })
    }

    fn visit_expr_binary(
        &mut self,
        operator: &Token,
        left: &Expr,
        right: &Expr,
    ) -> Result<Literal, RuntimeError> {
        let (right, left) = (self.visit_expression(right)?, self.visit_expression(left)?);
        let l = operator.line;
        let result = match operator.t_type {
            TokenType::Greater => Literal::Boolean(left.to_num(l)? > right.to_num(l)?),
            TokenType::GreaterEqual => Literal::Boolean(left.to_num(l)? >= right.to_num(l)?),
            TokenType::Less => Literal::Boolean(left.to_num(l)? < right.to_num(l)?),
            TokenType::LessEqual => Literal::Boolean(left.to_num(l)? <= right.to_num(l)?),

            TokenType::EqualEqual => Literal::Boolean(left == right),
            TokenType::BangEqual => Literal::Boolean(left != right),

            TokenType::Minus => Literal::Numeric(left.to_num(l)? - right.to_num(l)?),
            TokenType::Slash => Literal::Numeric(left.to_num(l)? / right.to_num(l)?),
            TokenType::Star => Literal::Numeric(left.to_num(l)? * right.to_num(l)?),
            TokenType::Plus => match (&left, &right) {
                (Literal::Numeric(l), Literal::Numeric(r)) => Literal::Numeric(l + r),
                (Literal::Letters(l), Literal::Letters(r)) => Literal::Letters(format!("{l}{r}")),
                _ => {
                    return Err(RuntimeError::Error {
                        line: l,
                        msg: format!("Can't add {right} to {left}"),
                    })
                }
            },
            _ => Literal::Null,
        };
        Ok(result)
    }

    fn visit_expr_logical(
        &mut self,
        operator: &Token,
        left: &Expr,
        right: &Expr,
    ) -> Result<Literal, RuntimeError> {
        let left = self.visit_expression(left)?;
        if operator.t_type != TokenType::Or && left.is_truthy() {
            self.visit_expression(right)
        } else {
            Ok(left)
        }
    }

    fn visit_expr_call(
        &mut self,
        callee: &Expr,
        paren: &Token,
        args: &[Expr],
    ) -> Result<Literal, RuntimeError> {
        let callee = self.visit_expression(callee)?;
        let mut arguments = vec![];
        for arg in args {
            arguments.push(self.visit_expression(arg)?);
        }
        let Literal::Callable(function) = callee else {
            return Err(RuntimeError::Error {
                line: paren.line,
                msg: "Can only call function and classes".to_owned(),
            });
        };
        // println!("function name {}", function);
        // println!("function arity {}", function.get_arity());
        // println!("function args {:?}", arguments);
        let (lenght, arity) = (arguments.len(), function.get_arity());
        if lenght != arity {
            return Err(RuntimeError::Error {
                line: paren.line,
                msg: format!("Expected {} arguments, got {}", arity, lenght),
            });
        }
        function.call(self, arguments)
    }

    fn visit_expr_get(&mut self, object: &Expr, name: &Token) -> Result<Literal, RuntimeError> {
        let object = self.visit_expression(object)?;
        if let Literal::Class(instance) = object {
            LoxInstance::get(instance, name)
        } else {
            Err(RuntimeError::Error {
                line: name.line,
                msg: "Only instances have properties".to_owned(),
            })
        }
    }

    fn visit_expr_set(
        &mut self,
        object: &Expr,
        name: &Token,
        value: &Expr,
    ) -> Result<Literal, RuntimeError> {
        let object = self.visit_expression(object)?;
        if let Literal::Class(instance) = object {
            let value = self.visit_expression(value)?;
            instance.borrow_mut().set(name, &value);
            Ok(value)
        } else {
            Err(RuntimeError::Error {
                line: name.line,
                msg: "Only instances have fields".to_owned(),
            })
        }
    }

    fn visit_expr_this(
        &mut self,
        expression: &Expr,
        keyword: &Token,
    ) -> Result<Literal, RuntimeError> {
        match self.locals.get(expression) {
            Some(distance) => self.graph.get_at(*distance, keyword),
            None => self.graph.envs[0].get_literal(keyword),
        }
    }

    fn visit_expr_super(
        &mut self,
        expression: &Expr,
        keyword: &Token,
        method: &Token,
    ) -> Result<Literal, RuntimeError> {
        let distance = self
            .locals
            .get(expression)
            .expect("'super' expression not found in interpreter locals");

        let superclass_literal = self.graph.get_at(*distance, keyword)?;
        let superclass = superclass_literal.get_class().unwrap();
        let token = Token::new_this(keyword.line);
        let instance = self.graph.get_at(distance - 1usize, &token)?;
        let Literal::Class(instance) = instance else {
            panic!("'this' not found at distance {distance}");
        };

        let func = superclass.find_method(&method.lexeme);

        let Some(function) = func else {
            return Err(RuntimeError::Error {
                line: method.line,
                msg: format!("undefined property '{}'.", method.lexeme),
            });
        };
        let function = function.bind(instance);
        Ok(Literal::new_function(function))
    }
}
