use crate::error::{ErrorType, JackError};
use crate::lexer::TokenData;
use std::io;
use std::io::Write;

pub fn parse(tokens: Vec<TokenData>) -> Result<String, JackError> {
    let parse_tree = String::new();

    for token in tokens {
        print!("<{:?}>", token.token_type);
        io::stdout().flush().unwrap();
        print!(
            "{}",
            token.token_str.unwrap_or(format!("{:?}", token.value))
        );
        io::stdout().flush().unwrap();
        println!("</{:?}>", token.token_type);
    }

    Ok(parse_tree)
}
