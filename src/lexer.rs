use crate::{
    error::{ErrorType, JackError},
    syntax_analyzer::{FileData, DEBUG},
};

#[derive(Debug, PartialEq, Eq, Clone)]
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

impl Token {
    pub fn is_type(&self) -> bool {
        matches!(
            self,
            Token::Int | Token::Char | Token::Boolean | Token::Identifier
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType {
    Keyword,
    Symbol,
    Identifier,
    IntConst,
    StringConst,
    Garbage,
}

#[derive(Debug, Clone)]
pub struct TokenData {
    pub token_type: TokenType,
    pub token: Token,
    pub token_str: Option<String>,
    pub path: String,
    pub line: u64,
    pub column: u16,
}

impl TokenData {
    fn new(
        token_type: TokenType,
        token: Token,
        token_str: Option<String>,
        path: String,
        line: u64,
        column: u16,
    ) -> Self {
        Self {
            token_type,
            token,
            token_str,
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

            let start_index = self.index;

            let mut token_str: Option<String> = None;
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
                b'/' => {
                    let next_tok = self.peek();
                    if next_tok == b'/' || next_tok == b'*' {
                        self.lex_comment()?;
                        continue;
                    }
                    (Token::BackSlash, TokenType::Symbol)
                }
                b'&' => (Token::Ampersand, TokenType::Symbol),
                b'|' => (Token::Pipe, TokenType::Symbol),
                b'<' => (Token::LessThan, TokenType::Symbol),
                b'>' => (Token::GreaterThan, TokenType::Symbol),
                b'=' => (Token::Equal, TokenType::Symbol),
                b'~' => (Token::Tilde, TokenType::Symbol),
                b'"' => {
                    let (tok, tok_type, tok_str) = self.lex_string_constant()?;
                    token_str = Some(tok_str);
                    (tok, tok_type)
                }
                _ => {
                    if current_byte.is_ascii_alphabetic() {
                        let (tok, tok_type, tok_str) = self.lex_keyword_or_identifier();
                        token_str = tok_str;
                        (tok, tok_type)
                    } else if current_byte.is_ascii_digit() {
                        let (tok, tok_type, int_str) = self.lex_integer_constant()?;
                        token_str = Some(int_str);
                        (tok, tok_type)
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
                token_str,
                self.file.path.clone(),
                self.file.line,
                self.file.column,
            ));

            if start_index == self.index {
                self.index += 1;
                self.file.column += 1;
            }
        }

        Ok(tokens)
    }

    fn peek(&self) -> u8 {
        self.file.file_contents.as_bytes()[self.index + 1]
    }

    fn peek_behind(&self) -> u8 {
        assert!(self.index > 0);
        self.file.file_contents.as_bytes()[self.index - 1]
    }

    fn eof(&self) -> bool {
        self.index >= self.file.file_contents.as_bytes().len()
    }

    fn lex_keyword_or_identifier(&mut self) -> (Token, TokenType, Option<String>) {
        let input = self.file.file_contents.as_bytes();

        let mut builder = String::new();
        while !input[self.index].is_ascii_whitespace() && !is_symbol(input[self.index]) {
            builder.push(input[self.index] as char);
            self.file.column += 1;
            self.index += 1;
        }

        match builder.as_str() {
            "class" => (Token::Class, TokenType::Keyword, Some(builder)),
            "constructor" => (Token::Constructor, TokenType::Keyword, Some(builder)),
            "function" => (Token::Function, TokenType::Keyword, Some(builder)),
            "method" => (Token::Method, TokenType::Keyword, Some(builder)),
            "field" => (Token::Field, TokenType::Keyword, Some(builder)),
            "static" => (Token::Static, TokenType::Keyword, Some(builder)),
            "var" => (Token::Var, TokenType::Keyword, Some(builder)),
            "int" => (Token::Int, TokenType::Keyword, Some(builder)),
            "char" => (Token::Char, TokenType::Keyword, Some(builder)),
            "boolean" => (Token::Boolean, TokenType::Keyword, Some(builder)),
            "void" => (Token::Void, TokenType::Keyword, Some(builder)),
            "true" => (Token::True, TokenType::Keyword, Some(builder)),
            "false" => (Token::False, TokenType::Keyword, Some(builder)),
            "null" => (Token::Null, TokenType::Keyword, Some(builder)),
            "this" => (Token::This, TokenType::Keyword, Some(builder)),
            "let" => (Token::Let, TokenType::Keyword, Some(builder)),
            "do" => (Token::Do, TokenType::Keyword, Some(builder)),
            "if" => (Token::If, TokenType::Keyword, Some(builder)),
            "else" => (Token::Else, TokenType::Keyword, Some(builder)),
            "while" => (Token::While, TokenType::Keyword, Some(builder)),
            "return" => (Token::Return, TokenType::Keyword, Some(builder)),
            _ => (Token::Identifier, TokenType::Identifier, Some(builder)),
        }
    }

    fn lex_comment(&mut self) -> Result<(), JackError> {
        match self.peek() {
            b'/' => self.advance_to_next_line(),
            b'*' => {
                let start_line = self.file.line;
                let start_column = self.file.column;

                // Move from '/' to '*' in '/*'.
                self.advance_to_next(b'*');
                // Find the '*' in '*/'.
                while !self.eof() && self.peek() != b'/' {
                    self.advance_to_next(b'*');
                }

                if self.eof() {
                    return Err(JackError::new(
                        ErrorType::GarbageToken,
                        "Unclosed multi-line comment.",
                        Some(self.file.path.clone()),
                        Some(start_line),
                        Some(start_column),
                    ));
                }

                // Advance past the '*/' multi-line closer.
                self.index += 2;
                self.file.column += 2;
            }
            _ => {
                return Err(JackError::new(
                    ErrorType::GarbageToken,
                    "Single backslash encountered. Did you mean '//'?",
                    Some(self.file.path.clone()),
                    Some(self.file.line),
                    Some(self.file.column),
                ));
            }
        }
        Ok(())
    }

    fn lex_integer_constant(&mut self) -> Result<(Token, TokenType, String), JackError> {
        let input = self.file.file_contents.as_bytes();

        let mut integer_builder = String::new();
        while !input[self.index].is_ascii_whitespace() && !is_symbol(input[self.index]) {
            integer_builder.push(input[self.index] as char);
            self.index += 1;
            self.file.column += 1;
        }

        for (i, ch) in integer_builder.as_bytes().iter().enumerate() {
            if !ch.is_ascii_digit() {
                return Err(JackError::new(
                    ErrorType::InvalidInteger,
                    "Invalid integer constant.",
                    Some(self.file.path.clone()),
                    Some(self.file.line),
                    Some(self.file.column - (integer_builder.len() as u16 - i as u16)),
                ));
            }
        }

        Ok((Token::IntegerConstant, TokenType::IntConst, integer_builder))
    }

    // TODO: FUTURE - Support multi line strings.
    fn lex_string_constant(&mut self) -> Result<(Token, TokenType, String), JackError> {
        let input = self.file.file_contents.as_bytes();

        // Store start of String to point in the case of an error.
        let start_column = self.file.column;
        let start_line = self.file.line;

        // We should currently be on the opening '"' so go to next token.
        self.index += 1;
        self.file.column += 1;

        let mut string_builder = String::new();
        while !self.eof() && input[self.index] != b'"' {
            if is_new_line(input[self.index]) {
                self.file.line += 1;
                self.file.column = 1;
            }
            string_builder.push(input[self.index] as char);
            self.index += 1;
            self.file.column += 1;
        }

        if self.eof() {
            return Err(JackError::new(
                ErrorType::UnclosedQuotation,
                "Non-terminated string. You forgot a '\"' somewhere.",
                Some(self.file.path.clone()),
                Some(start_line),
                Some(start_column),
            ));
        } else if self.file.line != start_line {
            return Err(JackError::new(
                ErrorType::GeneralError,
                "Multi-line strings are not supported at this time.",
                Some(self.file.path.clone()),
                Some(start_line),
                Some(start_column),
            ));
        }

        // Advance past the closing '"', otherwise it appears as the start of
        // another StringConstant.
        if input[self.index] == b'"' {
            self.index += 1;
            self.file.column += 1;
        }

        Ok((
            Token::StringConstant,
            TokenType::StringConst,
            string_builder,
        ))
    }

    fn advance_to_next(&mut self, byte: u8) {
        let file_bytes = self.file.file_contents.as_bytes();

        // Don't match the byte that we are already on. Skip it and find next.
        if file_bytes[self.index] == byte {
            if is_new_line(file_bytes[self.index]) {
                self.file.column = 1;
                self.file.line += 1;
            }
            self.file.column += 1;
            self.index += 1;
        }

        while !self.eof() && file_bytes[self.index] != byte {
            if is_new_line(file_bytes[self.index]) {
                self.file.column = 1;
                self.file.line += 1;
            }
            self.file.column += 1;
            self.index += 1;
        }
    }

    fn advance_to_next_line(&mut self) {
        while !is_new_line(self.file.file_contents.as_bytes()[self.index]) {
            self.index += 1;
        }
        // Advance to start of next line.
        self.index += 1;
        self.file.column = 1;
        self.file.line += 1;
    }
}

fn is_symbol(byte: u8) -> bool {
    let symbols = vec![
        b'{', b'}', b'(', b')', b'[', b']', b'.', b',', b';', b'+', b'-', b'*', b'/', b'&', b'|',
        b'<', b'>', b'=', b'~', b'"',
    ];

    if symbols.contains(&byte) {
        return true;
    }

    false
}

fn is_new_line(byte: u8) -> bool {
    byte == b'\r' || byte == b'\n'
}

#[cfg(test)]
mod lexer_tests {
    use super::*;

    #[test]
    fn test_is_new_line() {
        let valid_bytes = vec![b'\r', b'\n'];
        for byte in valid_bytes {
            assert!(is_new_line(byte));
        }

        let invalid_bytes = "ab83[}0817dHIjas\t";
        for byte in invalid_bytes.as_bytes() {
            assert!(!is_new_line(*byte));
        }
    }
}
