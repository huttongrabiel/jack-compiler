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

    fn parse_class(&mut self) {}

    fn parse_class_var_dev(&mut self) {}

    // Subroutine can be a method, function, or constructor
    fn parse_subroutine(&mut self) {}

    fn parse_parameter_list(&mut self) {}

    fn parse_var_dec(&mut self) {}

    fn parse_statements(&mut self) {}

    fn parse_do(&mut self) {}

    fn parse_let(&mut self) {}

    fn parse_while(&mut self) {}

    fn parse_return(&mut self) {}

    fn parse_if(&mut self) {}

    fn parse_expression(&mut self) {}

    // Will require a peek() function to see type of next token. This is to
    // distinguish between foo, foo[i], foo.print(), etc.
    fn parse_term(&mut self) {}

    // Expression lists are lists of expressions separated by commas. They CAN
    // be empty.
    fn parse_expression_list(&mut self) {}
}
