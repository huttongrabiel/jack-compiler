use std::fmt::Display;

#[derive(Debug)]
pub enum TokenType {
    KeyWord,
    Symbol,
    Identifier,
    IntVal,
    StringVal,
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let token_name: &str;
        match self {
            TokenType::KeyWord => token_name = "KeyWord",
            TokenType::Symbol => token_name = "Symbol",
            TokenType::Identifier => token_name = "Identifier",
            TokenType::IntVal => token_name = "IntVal",
            TokenType::StringVal => token_name = "StringVal",
        }
        write!(f, "{}", token_name.to_string())
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    pub path: String,
    pub line: u64,
    pub column: u16,
}

pub fn lex(file_contents: String) -> Result<Vec<Token>, &'static str> {
    let tokens: Vec<Token> = Vec::new();

    for byte in file_contents.as_bytes() {
        println!("\"{}\"", byte);
    }

    Ok(tokens)
}
