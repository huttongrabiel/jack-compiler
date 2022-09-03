use crate::lexer::TokenData;

pub fn parse(tokens: Vec<TokenData>) -> Result<String, &'static str> {
    let parse_tree = String::new();

    for token in tokens {
        print!("<{}>", token.token_type);
        print!("{}", token.value);
        println!("</{}>", token.token_type);
    }

    Ok(parse_tree)
}
