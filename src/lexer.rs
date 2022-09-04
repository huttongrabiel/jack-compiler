use crate::syntax_analyzer::{FileData, DEBUG};

#[derive(Debug, PartialEq, Eq)]
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
    Garbage,
}

#[derive(Debug)]
pub enum TokenType {
    KeyWord,
    Symbol,
    Identifier,
    IntVal,
    StringVal,
    Garbage,
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
    pub fn new(file: FileData) -> Self {
        Self { file, index: 0 }
    }

    pub fn lex(&mut self) -> Result<Vec<TokenData>, &'static str> {
        let mut tokens: Vec<TokenData> = Vec::new();

        let file_contents = self.file.file_contents.as_bytes();

        while !self.eof() {
            let current_byte = file_contents[self.index];

            if current_byte.is_ascii_whitespace() {
                // TODO: Increment file line count when current_byte is newline
                self.index += 1;
                continue;
            }
                // FIXME: Figure out how to check for keywords
                b'{' => Token::OpenCurly,
                b'}' => Token::CloseCurly,
                b'(' => Token::OpenParen,
                b')' => Token::CloseParen,
                b'[' => Token::OpenBracket,
                b']' => Token::CloseBracket,
                b'.' => Token::Dot,
                b',' => Token::Comma,
                b';' => Token::Semicolon,
                b'+' => Token::Plus,
                b'-' => Token::Minus,
                b'*' => Token::Asterik,
                b'/' => Token::BackSlash,
                b'&' => Token::Ampersand,
                b'|' => Token::Pipe,
                b'<' => Token::LessThan,
                b'>' => Token::GreaterThan,
                b'=' => Token::Equal,
                b'~' => Token::Tilde,
                _ => return Err("Invalid token"),
            };

            if !DEBUG && token == Token::Garbage {
                // TODO: Return error information (file, line, and column) of
                // the garbage token.
                return Err("Invalid token encountered");
            }

            tokens.push(TokenData::new(
                token_type,
                token,
                self.file.path.clone(),
                self.file.line,
                self.file.column,
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
        self.index >= self.file.file_contents.as_bytes().len()
    }
}
