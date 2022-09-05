use crate::{
    error::{ErrorType, JackError},
    syntax_analyzer::{FileData, DEBUG},
};

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

    pub fn lex(&mut self) -> Result<Vec<TokenData>, JackError> {
        let mut tokens: Vec<TokenData> = Vec::new();

        while !self.eof() {
            let current_byte = self.file.file_contents.as_bytes()[self.index];

            if current_byte.is_ascii_whitespace() {
                self.index += 1;
                self.file.column += 1;

                if is_new_line(current_byte) {
                    self.file.column = 1;
                    self.file.line += 1;
                }

                continue;
            }

            let (token, token_type) = match current_byte {
                b'{' => (Token::OpenCurly, TokenType::Symbol),
                b'}' => (Token::CloseCurly, TokenType::Symbol),
                b'(' => (Token::OpenParen, TokenType::Symbol),
                b')' => (Token::CloseParen, TokenType::Symbol),
                b'[' => (Token::OpenBracket, TokenType::Symbol),
                b']' => (Token::CloseBracket, TokenType::Symbol),
                b'.' => (Token::Dot, TokenType::Symbol),
                b',' => (Token::Comma, TokenType::Symbol),
                b';' => (Token::Semicolon, TokenType::Symbol),
                b'+' => (Token::Plus, TokenType::Symbol),
                b'-' => (Token::Minus, TokenType::Symbol),
                b'*' => (Token::Asterik, TokenType::Symbol),
                b'/' => (Token::BackSlash, TokenType::Symbol),
                b'&' => (Token::Ampersand, TokenType::Symbol),
                b'|' => (Token::Pipe, TokenType::Symbol),
                b'<' => (Token::LessThan, TokenType::Symbol),
                b'>' => (Token::GreaterThan, TokenType::Symbol),
                b'=' => (Token::Equal, TokenType::Symbol),
                b'~' => (Token::Tilde, TokenType::Symbol),
                b'"' => self.lex_string_constant(),
                _ => {
                    if current_byte.is_ascii_alphabetic() {
                        self.lex_keyword_or_identifier()
                    } else if current_byte.is_ascii_digit() {
                        self.lex_integer_constant()
                    } else {
                        return Err(JackError::new(
                            ErrorType::GarbageToken,
                            "Invalid token encountered",
                            Some(self.file.path.clone()),
                            Some(self.file.line),
                            Some(self.file.column),
                        ));
                    }
                }
            };

            if !DEBUG && token == Token::Garbage {
                return Err(JackError::new(
                    ErrorType::GarbageToken,
                    "Invalid token encountered",
                    Some(self.file.path.clone()),
                    Some(self.file.line),
                    Some(self.file.column),
                ));
            }

            tokens.push(TokenData::new(
                token_type,
                token,
                self.file.path.clone(),
                self.file.line,
                self.file.column,
            ));

            self.index += 1;
            self.file.column += 1;
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

    fn lex_keyword_or_identifier(&self) -> (Token, TokenType) {
        let start = self.index;
        let input = self.file.file_contents.as_bytes();

        if input[self.index].is_ascii_digit() {
            self.lex_integer_constant();
        }

        (Token::Garbage, TokenType::Garbage)
    }

    fn lex_integer_constant(&self) -> (Token, TokenType) {
        todo!()
    }

    fn lex_string_constant(&self) -> (Token, TokenType) {
        todo!()
    }
}

fn is_new_line(byte: u8) -> bool {
    byte == b'\r' || byte == b'\n'
}
