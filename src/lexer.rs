use crate::syntax_analyzer::FileData;
use std::fmt::Display;

#[derive(Debug)]
pub enum Token {
    // Keywords
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,

    // Symbols
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Dot,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Asterik,
    BackSlash,
    Ampersand,
    Pipe,
    LessThan,
    GreaterThan,
    Equal,
    Tilde,
    DoubleQuote,
    IntegerConstant,

    StringConstant,
    Identifier,
}

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
pub struct TokenData {
    pub token_type: TokenType,
    pub value: Token,
    pub path: String,
    pub line: u64,
    pub column: u16,
}

impl TokenData {
    fn new(
        token_type: TokenType,
        value: Token,
        path: String,
        line: u64,
        column: u16,
    ) -> Self {
        Self {
            token_type,
            value,
            path,
            line,
            column,
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    file: FileData,
    index: usize,
}

impl Lexer {
    pub fn lex(&self) -> Result<Vec<TokenData>, &'static str> {
        let mut tokens: Vec<TokenData> = Vec::new();

        let file_contents = self.file.file_contents.as_bytes();

        while self.eof() {
            let token = match &file_contents[self.index] {
                // FIXME: Figure out how to check for keywords
                b'{' => Token::OpenCurly,
                _ => return Err("Invalid token"),
            };

            tokens.push(TokenData::new(
                // FIXME: Get actual token type based on the token value
                TokenType::Symbol,
                Token::If,
                String::from("FIXME.txt"),
                20000,
                20000,
            ));

            self.index += 1;
        }

        Ok(tokens)
    }

    fn peek(&self) -> u8 {
        self.file.file_contents.as_bytes()[self.index + 1]
    }

    fn peek_k(&self, k: usize) -> u8 {
        self.file.file_contents.as_bytes()[self.index + k]
    }

    fn eof(&self) -> bool {
        self.index > self.file.file_contents.as_bytes().len()
    }
}
