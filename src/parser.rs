use crate::error::{ErrorType, JackError};
use crate::lexer::TokenData;

pub struct Parser {
    pub tokens: Vec<TokenData>,
    pub token_index: u64,
}

impl Parser {
    pub fn new(tokens: Vec<TokenData>) -> Self {
        Self {
            tokens,
            token_index: 0,
        }
    }

    pub fn parse(&mut self) -> Result<String, JackError> {
        let mut parse_tree = String::new();

        for token in &self.tokens {
            parse_tree.push_str(format!("<{:?}>", token.token_type).as_str());
            parse_tree.push_str(
                token
                    .token_str
                    .as_ref()
                    .unwrap_or(&format!("{:?}", token.value))
                    .as_str(),
            );
            parse_tree
                .push_str(format!("</{:?}>\n", token.token_type).as_str());
        }

        Ok(parse_tree)
    }
}
