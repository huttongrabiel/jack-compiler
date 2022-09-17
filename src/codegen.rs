use crate::{lexer::Token, parser::Segment};
use std::fmt::Write;

pub fn gen_push(segment: Segment, index: u32) -> String {
    let mut push = String::from("push ");
    writeln!(push, "{:?} {}", segment, index);
    push
}

pub fn gen_pop(segment: Segment, index: u32) -> String {
    let mut pop = String::from("pop ");
    writeln!(pop, "{:?} {}", segment, index);
    pop
}

pub fn gen_arithmetic(command: Token) -> String {
    let mut arithmetic_op = "";
    match command {
        Token::Plus => arithmetic_op = "add\n",
        // FIXME: Figure out how to allow negating a number. Right now the
        // negation won't work because all '-' are treated as sub.
        Token::Minus => arithmetic_op = "sub\n",
        Token::Equal => arithmetic_op = "eq\n",
        Token::GreaterThan => arithmetic_op = "gt\n",
        Token::LessThan => arithmetic_op = "lt\n",
        Token::Ampersand => arithmetic_op = "and\n",
        Token::Pipe => arithmetic_op = "or\n",
        Token::Tilde => arithmetic_op = "not\n",
        _ => panic!("Non-arithmetic token given to gen_arithmetic."),
    }

    arithmetic_op.to_string()
}

pub fn gen_label(label: String) -> String {
    format!("label {}\n", label)
}

pub fn gen_goto(label: String) -> String {
    format!("goto {}\n", label)
}

pub fn gen_if(label: String) -> String {
    format!("if-goto {}\n", label)
}

pub fn gen_call(name: String, n_args: u32) -> String {
    format!("call {} {}\n", name, n_args)
}

pub fn gen_function(name: String, n_locals: u32) -> String {
    format!("function {} {}\n", name, n_locals)
}

pub fn gen_return() -> String {
    String::from("return\n")
}
