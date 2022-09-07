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
            match token.token_type {
                TokenType::Keyword => match token.value {
                    Token::Class => {}
                    Token::Constructor => {}
                    Token::Function => {}
                    Token::Method => {}
                    Token::Field => {}
                    Token::Static => {}
                    Token::Var => {}
                    Token::Int => {}
                    Token::Char => {}
                    Token::Boolean => {}
                    Token::Void => {}
                    Token::True => {}
                    Token::False => {}
                    Token::Null => {}
                    Token::This => {}
                    Token::Let => {}
                    Token::Do => {}
                    Token::If => {}
                    Token::Else => {}
                    Token::While => {}
                    Token::Return => {}
                    _ => panic!(
                        "LEXER IS NOT CORRECT! HAS NON KEYWORD WRAPPED IN\
                            KEYWORD TYPE"
                    ),
                },
                // I think for the most part these will get skipped because they
                // will be advanced past in the parse... functions.
                TokenType::Symbol => {
                    match token.value {
                        Token::OpenCurly => {}
                        Token::CloseCurly => {}
                        Token::OpenParen => {}
                        Token::CloseParen => {}
                        Token::OpenBracket => {}
                        Token::CloseBracket => {}
                        Token::Dot => {}
                        Token::Comma => {}
                        Token::Semicolon => {}
                        Token::Plus => {}
                        Token::Minus => {}
                        Token::Asterik => {}
                        Token::BackSlash => {}
                        Token::Ampersand => {}
                        Token::Pipe => {}
                        Token::LessThan => {}
                        Token::GreaterThan => {}
                        Token::Equal => {}
                        Token::Tilde => {}
                        Token::DoubleQuote => {}
                        Token::IntegerConstant => {}
                        _ => return Err(JackError::new(
                            ErrorType::GeneralError,
                            "LEXER IS NOT CORRECT! HAS NON SYMBOL WRAPPED IN\
                            SYMBOL TYPE",
                            None,
                            None,
                            None,
                        )),
                    }
                }
                TokenType::Identifier => {}
                TokenType::IntConst => {}
                TokenType::StringConst => {}
                TokenType::Garbage => {
                    return Err(JackError::new(
                        ErrorType::GarbageToken,
                        "Unknown token encountered.",
                        Some(token.path.clone()),
                        Some(token.line),
                        Some(token.column),
                    ))
                }
            };

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
