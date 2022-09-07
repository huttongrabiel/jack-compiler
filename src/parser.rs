use crate::error::{ErrorType, JackError};
use crate::lexer::TokenData;

pub fn parse(tokens: Vec<TokenData>) -> Result<String, JackError> {
    let mut parse_tree = String::new();

    for token in tokens {
        parse_tree.push_str(format!("<{:?}>", token.token_type).as_str());
        parse_tree.push_str(
            token
                .token_str
                .unwrap_or(format!("{:?}", token.value))
                .as_str(),
        );
        parse_tree.push_str(format!("</{:?}>\n", token.token_type).as_str());
    }

    Ok(parse_tree)
}
