use crate::{lexer::Token, parser::Segment};
use std::fmt::Write;

#[derive(Debug)]
pub struct Codegen {
    vm_code: String,
}

impl Codegen {
    pub fn new() -> Self {
        Self {
            vm_code: String::new(),
        }
    }

    fn gen_push(segment: Segment, index: u32) -> String {
        let mut push = String::from("push ");
        writeln!(push, "{:?} {}", segment, index);
        push
    }

    fn gen_pop(segment: Segment, index: u32) -> String {
        let mut pop = String::from("pop ");
        writeln!(pop, "{:?} {}", segment, index);
        pop
    }

    fn gen_arithmetic(command: Token) -> String {
        let mut arithmetic_op = "";
        match command {
            Token::Plus => arithmetic_op = "add",
            // FIXME: Figure out how to allow negating a number. Right now the
            // negation won't work because all '-' are treated as sub.
            Token::Minus => arithmetic_op = "sub",
            Token::Equal => arithmetic_op = "eq",
            Token::GreaterThan => arithmetic_op = "gt",
            Token::LessThan => arithmetic_op = "lt",
            Token::Ampersand => arithmetic_op = "and",
            Token::Pipe => arithmetic_op = "or",
            Token::Tilde => arithmetic_op = "not",
            _ => panic!("Non-arithmetic token given to gen_arithmetic."),
        }

        arithmetic_op.to_string()
    }

    fn gen_label(label: String) -> String {}

    fn gen_goto(label: String) -> String {}

    fn gen_if(label: String) -> String {}

    fn gen_call(name: String, n_args: u32) -> String {}

    fn gen_function(name: String, n_locals: u32) -> String {}

    fn gen_return() -> String {}
}
