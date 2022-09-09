use crate::error::{ErrorType, JackError};
use crate::lexer::{Token, TokenData, TokenType};

pub enum ParseTag {
    // 'class' className '{' ClassVarDec* SubroutineDec* '}'
    Class,
    // ('static' | 'field') Type VarName (',' VarName)* ';'
    ClassVarDec,
    // 'int' | 'char' | 'boolean' | ClassName
    Type,
    // ('constructor' | 'function' | 'method') ('void' | Type) SubroutineName
    // '(' ParameterList ')' SubroutineBody
    SubroutineDec,
    // ((Type VarName)(',' type VarName)*)?
    ParameterList,
    // '{' VarDec* statements '}'
    SubroutineBody,
    // 'var' Type VarName (',', VarName)* ';'
    VarDec,
    // Identifier
    ClassName,
    // Identifier
    SubroutineName,
    // Identifier
    VarName,
    // statement*
    Statements,
    // 'let' VarName ('[' Expression ']')? '=' Expression ';'
    LetStatement,
    // 'if' '(' Expression ')' '{' Statements '}' ('else' '{' Statements '}')?
    IfStatement,
    // 'while' '(' Expression ')' '{' Statements '}'
    WhileStatement,
    // do SubroutineCall ';'
    DoStatement,
    // 'return' expression? ';' (REMEMBER, EVERY function returns in Jack)
    ReturnStatement,
    // term (op term*)
    Expression,
    // IntegerConstant | StringConstant | KeywordConstant | VarName |
    // VarName '[' ExpressionList ']' | SubroutineCall | '(' expression ')' |
    // UnaryOp
    Term,
    // SubroutineName '(' ExpressionList ')' | (ClassName | VarName) '.'
    // SubroutineName '(' ExpressionList ')'
    SubroutineCall,
    // (expression (',' expression)*)?
    ExpressionList,
    // '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
    Op,
    // '-' | '~' // '~' is NOT operator idk why they didn't use '!'
    UnaryOp,
    // 'true' | 'false' | 'null' | 'this'
    KeywordConstant,
}

pub struct Parser {
    tokens: Vec<TokenData>,
    index: usize,
    indent_amount: usize,
}

impl Parser {
    pub fn new(tokens: Vec<TokenData>) -> Self {
        Self {
            tokens,
            index: 0,
            indent_amount: 0,
        }
    }

    pub fn parse(&mut self) -> Result<String, JackError> {
        if self.tokens[0].token != Token::Class {
            // FIXME: Consider adding an ErrorType for this.
            return Err(JackError::new(
                ErrorType::GeneralError,
                "All jack files must begin with a class declaration.",
                Some(self.tokens[0].path.clone()),
                Some(1),
                Some(1),
            ));
        }

        let mut parse_tree = String::new();

        //  parse_class() is the jumping off point for our parser. It should
        //  call the functions it needs which in turn call other functions etc.
        //  But each jack file starts with a class so that is all we should need
        //  to call the begint he descent parsing.
        parse_tree.push_str(&self.parse_class()?);

        //        while self.has_more_tokens() {
        //            let start_index = self.index;
        //
        //            let current_token = self.current_token().clone();
        //            match current_token.token_type {
        //                TokenType::Keyword => match current_token.token {
        //                    Token::Class => self.parse_class(),
        //                    // FIXME: Should parse_subroutine take an enum specifying
        //                    // the subroutine type? Something like Subroutine::Method.
        //                    Token::Constructor | Token::Function | Token::Method => {
        //                        self.parse_subroutine()
        //                    }
        //                    Token::Field | Token::Static => self.parse_class_var_dec(),
        //                    Token::Var => self.parse_var_dec(),
        //                    // These blank ones are going only going to occur within
        //                    // the parse... methods so they probably won't end up ever
        //                    // being triggered here.
        //                    Token::Int => {}
        //                    Token::Char => {}
        //                    Token::Boolean => {}
        //                    Token::Void => {}
        //                    Token::True => {}
        //                    Token::False => {}
        //                    Token::Null => {}
        //                    Token::This => {}
        //                    Token::Let => self.parse_let(),
        //                    Token::Do => self.parse_do(),
        //                    Token::If | Token::Else => self.parse_if(),
        //                    Token::While => self.parse_while(),
        //                    Token::Return => self.parse_return(),
        //                    _ => {
        //                        return Err(JackError::new(
        //                            ErrorType::GeneralError,
        //                            "LEXER IS NOT CORRECT! HAS NON KEYWORD WRAPPED IN \
        //                            KEYWORD TYPE",
        //                            Some(current_token.path.clone()),
        //                            Some(current_token.line),
        //                            Some(current_token.column),
        //                        ));
        //                    }
        //                },
        //                // I think for the most part these will get skipped because they
        //                // will be advanced past in the parse... functions.
        //                TokenType::Symbol => {
        //                    match current_token.token {
        //                        Token::OpenCurly => {}
        //                        Token::CloseCurly => {}
        //                        Token::OpenParen => {}
        //                        Token::CloseParen => {}
        //                        Token::OpenBracket => {}
        //                        Token::CloseBracket => {}
        //                        Token::Dot => {}
        //                        Token::Comma => {}
        //                        Token::Semicolon => {}
        //                        Token::Plus => {}
        //                        Token::Minus => {}
        //                        Token::Asterik => {}
        //                        Token::BackSlash => {}
        //                        Token::Ampersand => {}
        //                        Token::Pipe => {}
        //                        Token::LessThan => {}
        //                        Token::GreaterThan => {}
        //                        Token::Equal => {}
        //                        Token::Tilde => {}
        //                        Token::DoubleQuote => {}
        //                        Token::IntegerConstant => {}
        //                        _ => return Err(JackError::new(
        //                            ErrorType::GeneralError,
        //                            "LEXER IS NOT CORRECT! HAS NON SYMBOL WRAPPED IN \
        //                            SYMBOL TYPE",
        //                            Some(current_token.path.clone()),
        //                            Some(current_token.line),
        //                            Some(current_token.column),
        //                        )),
        //                    }
        //                }
        //                TokenType::Identifier => {}
        //                TokenType::IntConst => {}
        //                TokenType::StringConst => {}
        //                TokenType::Garbage => {
        //                    return Err(JackError::new(
        //                        ErrorType::GarbageToken,
        //                        "Unknown token encountered.",
        //                        Some(current_token.path.clone()),
        //                        Some(current_token.line),
        //                        Some(current_token.column),
        //                    ))
        //                }
        //            };
        //
        //            parse_tree
        //                .push_str(format!("<{:?}>", current_token.token_type).as_str());
        //            parse_tree.push_str(
        //                current_token
        //                    .token_str
        //                    .as_ref()
        //                    .unwrap_or(&format!("{:?}", current_token.token))
        //                    .as_str(),
        //            );
        //            parse_tree.push_str(
        //                format!("</{:?}>\n", current_token.token_type).as_str(),
        //            );
        //
        //            if start_index == self.index {
        //                self.index += 1;
        //            }
        //        }

        Ok(parse_tree)
    }

    fn has_more_tokens(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn current_token(&self) -> &TokenData {
        &self.tokens[self.index]
    }

    fn parse_class(&mut self) -> Result<String, JackError> {
        let mut class_parse_tree = String::from("<class>\n");

        class_parse_tree
            .push_str(&self.generate_xml_tag(&self.current_token()));

        self.indent_amount += 2;

        self.index += 1;

        if self.current_token().token_type != TokenType::Identifier {
            return Err(JackError::new(
                ErrorType::GeneralError,
                "Expect identifier. 'class _identifer_ {...}'",
                Some(self.current_token().path.clone()),
                Some(self.current_token().line),
                Some(self.current_token().column),
            ));
        }
        self.index += 1;

        if self.current_token().token != Token::OpenCurly {
            return Err(JackError::new(
                ErrorType::GeneralError,
                "Expected '{'.",
                Some(self.current_token().path.clone()),
                Some(self.current_token().line),
                Some(self.current_token().column),
            ));
        }

        // Move to the first token of the body of the class.
        self.index += 1;

        class_parse_tree.push_str(&self.parse_class_var_dec()?);
        class_parse_tree.push_str(&self.parse_subroutine()?);

        // We should only return to this point once we have reached the end of
        // the class.
        if self.current_token().token != Token::CloseCurly {
            return Err(JackError::new(
                ErrorType::GeneralError,
                "Expected '}'.",
                Some(self.current_token().path.clone()),
                Some(self.current_token().line),
                Some(self.current_token().column),
            ));
        }

        class_parse_tree.push_str("</class>");

        Ok(class_parse_tree)
    }

    fn parse_class_var_dec(&mut self) -> Result<String, JackError> {
        if self.current_token().token != Token::Field
            && self.current_token().token != Token::Static
        {
            // Classes do not require having variable declarations.
            return Ok(String::from(""));
        }

        Ok(String::from("PLACEHOLDER"))
    }

    // Subroutine can be a method, function, or constructor
    fn parse_subroutine(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_parameter_list(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_var_dec(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_statements(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_do(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_let(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_while(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_return(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_if(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn parse_expression(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    // Will require a peek() function to see type of next token. This is to
    // distinguish between foo, foo[i], foo.print(), etc.
    fn parse_term(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    // Expression lists are lists of expressions separated by commas. They CAN
    // be empty.
    fn parse_expression_list(&mut self) -> Result<String, JackError> {
        Ok(String::from("PLACEHOLDER"))
    }

    fn generate_xml_tag(&self, token: &TokenData) -> String {
        let mut xml_tag = String::new();

        xml_tag.push_str(&self.generate_indent());
        xml_tag.push_str(format!("<{:?}>", token.token_type).as_str());
        xml_tag.push_str(
            token
                .token_str
                .as_ref()
                .unwrap_or(&format!("{:?}", token.token))
                .as_str(),
        );
        xml_tag.push_str(format!("</{:?}>\n", token.token_type).as_str());

        xml_tag
    }

    fn generate_indent(&self) -> String {
        " ".repeat(self.indent_amount)
    }
}
