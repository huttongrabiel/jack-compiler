use crate::codegen;
use crate::error::{ErrorType, JackError};
use crate::lexer::{Token, TokenData, TokenType};
use crate::symbol_table::{Kind, SymbolTable};
use std::fmt::Write;

#[derive(Debug)]
pub enum Segment {
    Const,
    Arg,
    Local,
    Static,
    This,
    That,
    Pointer,
    Temp,
}

#[derive(Debug)]
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
    class_symbol_table: SymbolTable,
    subroutine_symbol_table: SymbolTable,
}

impl Parser {
    pub fn new(tokens: Vec<TokenData>) -> Self {
        Self {
            tokens,
            index: 0,
            indent_amount: 0,
            class_symbol_table: SymbolTable::new(),
            subroutine_symbol_table: SymbolTable::new(),
        }
    }

    pub fn parse(&mut self) -> Result<(String, String), JackError> {
        if self.tokens[0].token != Token::Class {
            return Err(JackError::new(
                ErrorType::NoClassDeclaration,
                "All jack files must begin with a class declaration.",
                Some(self.tokens[0].path.clone()),
                Some(1),
                Some(1),
            ));
        }

        let mut parse_tree = String::new();
        let mut vm_code = String::new();

        //  parse_class() is the jumping off point for our parser. It should
        //  call the functions it needs which in turn call other functions etc.
        //  But each jack file starts with a class so that is all we should need
        //  to call the begin the descent parsing.
        let (parse_tree, vm_code) = self.parse_class()?;

        Ok((parse_tree, vm_code))
    }

    fn has_more_tokens(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn current_token(&self) -> &TokenData {
        &self.tokens[self.index]
    }

    fn peek(&self) -> &TokenData {
        assert!(self.has_more_tokens());
        &self.tokens[self.index + 1]
    }

    fn peek_k(&self, k: usize) -> &TokenData {
        assert!(self.index + k < self.tokens.len());
        &self.tokens[self.index + k]
    }

    fn peek_behind(&self) -> &TokenData {
        assert!(self.index > 0);
        &self.tokens[self.index - 1]
    }

    fn parse_class(&mut self) -> Result<(String, String), JackError> {
        // Output string for XML parse tree.
        let mut class_parse_tree = String::from("<class>\n");

        // Output string for VM code.
        let mut class_vm_code = String::new();

        self.indent_amount += 2;
        class_parse_tree.push_str(&self.generate_xml_tag());

        self.index += 1;

        if self.tokens[self.index].token_type != TokenType::Identifier {
            return Err(JackError::new(
                ErrorType::MissingIdentifier,
                "Expect identifier. 'class _identifer_ {...}'",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        class_parse_tree.push_str(&self.generate_xml_tag());

        self.index += 1;

        if self.tokens[self.index].token != Token::OpenCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '{'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        class_parse_tree.push_str(&self.generate_xml_tag());

        // Move to the first token of the body of the class.
        self.index += 1;

        let (xml_ast, local_vm_code) = &self.parse_class_var_dec()?;
        class_parse_tree.push_str(xml_ast);
        class_vm_code.push_str(&local_vm_code);

        let (xml_ast, local_vm_code) = &self.parse_subroutine()?;
        class_parse_tree.push_str(xml_ast);
        class_vm_code.push_str(&local_vm_code);

        // FIXME: Remove this commented out code.
        // Into the depths we go!
        //        class_parse_tree.push_str(&self.parse_class_var_dec()?);
        //        class_parse_tree.push_str(&self.parse_subroutine()?);

        // We should only return to this point once we have reached the end of
        // the class.
        if self.tokens[self.index].token != Token::CloseCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '}'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        class_parse_tree.push_str("</class>");

        Ok((class_parse_tree, class_vm_code))
    }

    fn parse_class_var_dec(&mut self) -> Result<(String, String), JackError> {
        if self.current_token().token != Token::Field && self.current_token().token != Token::Static
        {
            // Classes do not require having variable declarations.
            return Ok((String::new(), String::new()));
        }

        let mut var_dec_vm_code = String::new();

        let mut cvd_parse_tree = self.generate_indent();
        writeln!(cvd_parse_tree, "<{:?}>", ParseTag::ClassVarDec)
            .expect("Failed to write cvd_parse_tree.");

        self.indent_amount += 2;

        assert!(
            self.current_token().token == Token::Field
                || self.current_token().token == Token::Static
        );

        let mut kind: Kind;
        let mut segment: Segment;
        if self.current_token().token == Token::Field {
            kind = Kind::Field;
            segment = Segment::Local;
        } else {
            kind = Kind::Static;
            segment = Segment::Static;
        }

        cvd_parse_tree.push_str(&self.generate_xml_tag());

        self.index += 1;

        if self.tokens[self.index].token.is_type() {
            cvd_parse_tree.push_str(&self.generate_xml_tag());
        } else {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected type.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        let ty = format!("{:?}", self.current_token().token);
        self.index += 1;

        if self.tokens[self.index].token != Token::Identifier {
            return Err(JackError::new(
                ErrorType::MissingIdentifier,
                "Expected variable name.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        assert!(self.current_token().token_str.is_some());

        // We call the push before adding anything to the symbol table because
        // the index changes after the symbol is added to the table.
        var_dec_vm_code.push_str(&codegen::gen_push(segment, self.class_symbol_table.index));
        self.class_symbol_table
            .define(self.current_token().token_str.unwrap(), ty, kind);

        cvd_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        let (xml_ast, vm_code) = self.parse_multi_variable_declaration()?;
        cvd_parse_tree.push_str(&xml_ast);
        var_dec_vm_code.push_str(&vm_code);

        if self.tokens[self.index].token != Token::Semicolon {
            return Err(JackError::new(
                ErrorType::ExpectedSemicolon,
                "Expected ';'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        cvd_parse_tree.push_str(&self.generate_xml_tag());

        // Move past SemiColon.
        self.index += 1;
        self.indent_amount -= 2;

        cvd_parse_tree.push_str(&self.generate_indent());
        writeln!(cvd_parse_tree, "</{:?}>", ParseTag::ClassVarDec)
            .expect("Failed to write cvd_parse_tree.");

        if self.tokens[self.index].token == Token::Static
            || self.tokens[self.index].token == Token::Field
        {
            let (xml_ast, vm_code) = &self.parse_class_var_dec()?;
            var_dec_vm_code.push_str(vm_code);
            cvd_parse_tree.push_str(xml_ast);
        }

        Ok((var_dec_vm_code, cvd_parse_tree))
    }

    // Subroutine can be a method, function, or constructor
    fn parse_subroutine(&mut self) -> Result<String, JackError> {
        // Subroutines are not required in a class.
        match self.tokens[self.index].token {
            Token::Constructor | Token::Function | Token::Method => (),
            _ => return Ok((String::new(), String::new())),
        }

        let mut subroutine_parse_tree = self.generate_indent();

        // Parse start of SubroutineDec.
        writeln!(subroutine_parse_tree, "<{:?}>", ParseTag::SubroutineDec)
            .expect("Failed to write <SubroutineDec>.");

        self.indent_amount += 2;

        subroutine_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        match self.tokens[self.index].token {
            Token::Void | Token::Int | Token::Boolean | Token::Char | Token::Identifier => (),
            _ => {
                return Err(JackError::new(
                    ErrorType::NoReturn,
                    "Expected subroutine return type. All subroutines must return.",
                    Some(self.tokens[self.index].path.clone()),
                    Some(self.tokens[self.index].line),
                    Some(self.tokens[self.index].column),
                ));
            }
        }

        subroutine_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token != Token::Identifier {
            return Err(JackError::new(
                ErrorType::MissingIdentifier,
                "Expected subroutine name.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        subroutine_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token != Token::OpenParen {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '('.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        };

        subroutine_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        // Parse ParameterList.
        subroutine_parse_tree.push_str(&self.parse_parameter_list()?);

        if self.tokens[self.index].token != Token::CloseParen {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected ')'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        };

        subroutine_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        // Parse SubroutineBody.
        subroutine_parse_tree.push_str(&self.generate_indent());
        writeln!(subroutine_parse_tree, "<{:?}>", ParseTag::SubroutineBody)
            .expect("Failed to write <SubroutineBody>.");
        self.indent_amount += 2;

        if self.tokens[self.index].token != Token::OpenCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '{'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        subroutine_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        // We dive DEEPER into the depths!
        subroutine_parse_tree.push_str(&self.parse_var_dec()?);
        subroutine_parse_tree.push_str(&self.parse_statements()?);

        if self.tokens[self.index].token != Token::CloseCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '}'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        subroutine_parse_tree.push_str(&self.generate_xml_tag());

        self.indent_amount -= 2;
        subroutine_parse_tree.push_str(&self.generate_indent());
        writeln!(subroutine_parse_tree, "</{:?}>", ParseTag::SubroutineBody)
            .expect("Failed to write </SubroutineBody>.");

        // Close up SubroutineDec
        self.indent_amount -= 2;

        subroutine_parse_tree.push_str(&self.generate_indent());
        writeln!(subroutine_parse_tree, "</{:?}>", ParseTag::SubroutineDec)
            .expect("Failed to write </SubroutineDec>.");

        // Continue parsing other subroutines or reach end of class.
        self.index += 1;

        match self.tokens[self.index].token {
            Token::Constructor | Token::Function | Token::Method => {
                subroutine_parse_tree.push_str(&self.parse_subroutine()?)
            }
            Token::CloseCurly => subroutine_parse_tree.push_str(&self.generate_xml_tag()),
            _ => {
                return Err(JackError::new(
                    ErrorType::UnexpectedToken,
                    "Expected end of class or another subroutine. Found other.",
                    Some(self.tokens[self.index].path.clone()),
                    Some(self.tokens[self.index].line),
                    Some(self.tokens[self.index].column),
                ));
            }
        };

        Ok(subroutine_parse_tree)
    }

    fn parse_parameter_list(&mut self) -> Result<String, JackError> {
        if self.tokens[self.index].token == Token::CloseParen {
            let mut empty_parameter_list = self.generate_indent();
            writeln!(empty_parameter_list, "<{:?}>", ParseTag::ParameterList)
                .expect("Failed to write <ParameterList>.");
            empty_parameter_list.push_str(&self.generate_indent());
            writeln!(empty_parameter_list, "</{:?}>", ParseTag::ParameterList)
                .expect("Failed to write </ParameterList>.");
            return Ok(empty_parameter_list);
        }

        let mut parameter_list_parse_tree = self.generate_indent();
        writeln!(parameter_list_parse_tree, "<{:?}>", ParseTag::ParameterList)
            .expect("Failed to write <ParameterList>.");

        self.indent_amount += 2;

        if !self.tokens[self.index].token.is_type() {
            return Err(JackError::new(
                ErrorType::MissingType,
                "Expected type of var in parameter list.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        parameter_list_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token != Token::Identifier {
            return Err(JackError::new(
                ErrorType::MissingIdentifier,
                "Expected identifier in parameter list.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        parameter_list_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        // Handle parameter lists that have multiple parameters.
        while self.tokens[self.index].token != Token::CloseParen {
            if self.tokens[self.index].token == Token::Comma
                && self.peek().token.is_type()
                && self.peek_k(2).token == Token::Identifier
            {
                parameter_list_parse_tree.push_str(&self.generate_xml_tag());
                self.index += 1;
                parameter_list_parse_tree.push_str(&self.generate_xml_tag());
                self.index += 1;
                parameter_list_parse_tree.push_str(&self.generate_xml_tag());
            } else {
                return Err(JackError::new(
                    ErrorType::BadParameterList,
                    "Expected sequence of ', type VarName' in non-empty parameter list.",
                    Some(self.tokens[self.index].path.clone()),
                    Some(self.tokens[self.index].line),
                    Some(self.tokens[self.index].column),
                ));
            }
            self.index += 1;
        }

        self.indent_amount -= 2;

        parameter_list_parse_tree.push_str(&self.generate_indent());
        writeln!(
            parameter_list_parse_tree,
            "</{:?}>",
            ParseTag::ParameterList
        )
        .expect("Failed to write </ParameterList>.");

        Ok(parameter_list_parse_tree)
    }

    fn parse_var_dec(&mut self) -> Result<String, JackError> {
        // VarDec is optional is subroutine body.
        if self.tokens[self.index].token != Token::Var {
            return Ok(String::new());
        }

        let mut var_dec_parse_tree = self.generate_indent();
        writeln!(var_dec_parse_tree, "<{:?}>", ParseTag::VarDec)
            .expect("Failed to write <VarDec>.");

        self.indent_amount += 2;

        var_dec_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if !self.tokens[self.index].token.is_type() {
            return Err(JackError::new(
                ErrorType::MissingType,
                "Expected type in variable declaration.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        var_dec_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token != Token::Identifier {
            return Err(JackError::new(
                ErrorType::MissingIdentifier,
                "Expected identifer in variable declaration. 'var type _identifer_ ...'",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        var_dec_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        var_dec_parse_tree.push_str(&self.parse_multi_variable_declaration()?);

        if self.tokens[self.index].token != Token::Semicolon {
            return Err(JackError::new(
                ErrorType::ExpectedSemicolon,
                "Expected ';'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        var_dec_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        self.indent_amount -= 2;

        var_dec_parse_tree.push_str(&self.generate_indent());
        writeln!(var_dec_parse_tree, "</{:?}>", ParseTag::VarDec)
            .expect("Failed to write </VarDec>.");

        if self.tokens[self.index].token == Token::Var {
            var_dec_parse_tree.push_str(&self.parse_var_dec()?);
        }

        Ok(var_dec_parse_tree)
    }

    /// Multi variable declarations are variable declarations that happen on a single line.
    ///
    /// Example:
    ///     ```var int foo, bar, baz;```
    ///
    /// This function gets called after it has been checked that a 'var int foo'
    /// exists. It then gets called and continues parsing until it encounters a
    /// semi colon at which point it returns its parse tree.
    fn parse_multi_variable_declaration(&mut self) -> Result<String, JackError> {
        let mut multi_variable_declaration_parse_tree = String::new();

        while self.tokens[self.index].token != Token::Semicolon {
            if self.tokens[self.index].token == Token::Comma
                && self.peek().token == Token::Identifier
            {
                multi_variable_declaration_parse_tree.push_str(&self.generate_xml_tag());
                self.index += 1;
                multi_variable_declaration_parse_tree.push_str(&self.generate_xml_tag());
            } else {
                return Err(JackError::new(
                    ErrorType::BadMultiVariableDeclaration,
                    "Expected sequence of ', VarName' for single line multi variable declaration.",
                    Some(self.tokens[self.index].path.clone()),
                    Some(self.tokens[self.index].line),
                    Some(self.tokens[self.index].column),
                ));
            }
            self.index += 1;
        }

        Ok(multi_variable_declaration_parse_tree)
    }

    fn parse_statements(&mut self) -> Result<String, JackError> {
        // At the minimum a 'return;' statement is required.
        if self.tokens[self.index].token == Token::CloseCurly
            && self.peek_behind().token == Token::OpenCurly
        {
            return Err(JackError::new(
                ErrorType::NoReturn,
                "All subroutines, even blank ones, must return. Try adding \"return;\"",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        let mut statement_parse_tree = self.generate_indent();

        writeln!(statement_parse_tree, "<{:?}>", ParseTag::Statements)
            .expect("Failed to write <Statements>.");
        self.indent_amount += 2;

        while self.current_token().token.is_statement_keyword() {
            match self.tokens[self.index].token {
                Token::Let => statement_parse_tree.push_str(&self.parse_let()?),
                Token::If => statement_parse_tree.push_str(&self.parse_if()?),
                Token::While => statement_parse_tree.push_str(&self.parse_while()?),
                Token::Do => statement_parse_tree.push_str(&self.parse_do()?),
                Token::Return => statement_parse_tree.push_str(&self.parse_return()?),
                _ => {
                    return Err(JackError::new(
                        ErrorType::UnexpectedToken,
                        "Unexpected token in subroutine body.",
                        Some(self.tokens[self.index].path.clone()),
                        Some(self.tokens[self.index].line),
                        Some(self.tokens[self.index].column),
                    ));
                }
            };
        }

        self.indent_amount -= 2;
        statement_parse_tree.push_str(&self.generate_indent());
        writeln!(statement_parse_tree, "</{:?}>", ParseTag::Statements)
            .expect("Failed to write </Statements>.");

        Ok(statement_parse_tree)
    }

    fn parse_do(&mut self) -> Result<String, JackError> {
        let mut do_parse_tree = self.generate_indent();

        writeln!(do_parse_tree, "<{:?}>", ParseTag::DoStatement)
            .expect("Failed to write <DoStatement>.");
        self.indent_amount += 2;

        // Add the XML tag for 'do' to the parse tree.
        do_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        do_parse_tree.push_str(&self.parse_subroutine_call()?);

        if self.current_token().token != Token::Semicolon {
            return Err(JackError::new(
                ErrorType::ExpectedSemicolon,
                "Expected ';'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        do_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        self.indent_amount -= 2;
        do_parse_tree.push_str(&self.generate_indent());
        writeln!(do_parse_tree, "</{:?}>", ParseTag::DoStatement)
            .expect("Failed to write </DoStatement>.");

        Ok(do_parse_tree)
    }

    fn parse_let(&mut self) -> Result<String, JackError> {
        let mut let_parse_tree = self.generate_indent();

        writeln!(let_parse_tree, "<{:?}>", ParseTag::LetStatement)
            .expect("Failed to write <LetStatement>.");

        self.indent_amount += 2;

        // Adds the XML for 'let' onto the parse tree. If we are at this point
        // in the function, we are on a let, so we don't need to check it.
        let_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token != Token::Identifier {
            return Err(JackError::new(
                ErrorType::MissingIdentifier,
                "Expected identifier in let statement. 'let _identifer_ = ...'",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        let_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token == Token::OpenBracket {
            let_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;

            let_parse_tree.push_str(&self.parse_expression()?);

            if self.tokens[self.index].token != Token::CloseBracket {
                return Err(JackError::new(
                    ErrorType::UnexpectedToken,
                    "Expected ']'.",
                    Some(self.tokens[self.index].path.clone()),
                    Some(self.tokens[self.index].line),
                    Some(self.tokens[self.index].column),
                ));
            }

            let_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;
        }

        if self.tokens[self.index].token != Token::Equal {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '='.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        let_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        let_parse_tree.push_str(&self.parse_expression()?);

        if self.tokens[self.index].token != Token::Semicolon {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected ';'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        let_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        self.indent_amount -= 2;
        let_parse_tree.push_str(&self.generate_indent());
        writeln!(let_parse_tree, "</{:?}>", ParseTag::LetStatement)
            .expect("Failed to write </LetStatement>.");

        Ok(let_parse_tree)
    }

    fn parse_while(&mut self) -> Result<String, JackError> {
        let mut while_parse_tree = self.generate_indent();

        writeln!(while_parse_tree, "<{:?}>", ParseTag::WhileStatement)
            .expect("Failed to write <WhileStatement>.");
        self.indent_amount += 2;

        // Add the tag for 'while' onto the parse tree.
        while_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.current_token().token != Token::OpenParen {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '('.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        while_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        while_parse_tree.push_str(&self.parse_expression()?);

        if self.current_token().token != Token::CloseParen {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected ')'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        while_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.current_token().token != Token::OpenCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '{'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        while_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        while_parse_tree.push_str(&self.parse_statements()?);

        if self.current_token().token != Token::CloseCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '}'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        while_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        self.indent_amount -= 2;
        while_parse_tree.push_str(&self.generate_indent());
        writeln!(while_parse_tree, "</{:?}>", ParseTag::WhileStatement)
            .expect("Failed to write </WhileStatement>.");

        Ok(while_parse_tree)
    }

    fn parse_return(&mut self) -> Result<String, JackError> {
        let mut return_parse_tree = self.generate_indent();

        writeln!(return_parse_tree, "<{:?}>", ParseTag::ReturnStatement)
            .expect("Failed to write <ReturnStatement>.");
        self.indent_amount += 2;

        // Add the 'return' keyword to the parse tree.
        return_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.current_token().token != Token::Semicolon {
            return_parse_tree.push_str(&self.parse_expression()?);
        }

        // NOW we should be on a semicolon.
        if self.current_token().token != Token::Semicolon {
            return Err(JackError::new(
                ErrorType::ExpectedSemicolon,
                "Expected ';'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        return_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        self.indent_amount -= 2;
        return_parse_tree.push_str(&self.generate_indent());
        writeln!(return_parse_tree, "</{:?}>", ParseTag::ReturnStatement)
            .expect("Failed to write </ReturnStatement>.");

        Ok(return_parse_tree)
    }

    fn parse_if(&mut self) -> Result<String, JackError> {
        let mut if_parse_tree = self.generate_indent();

        writeln!(if_parse_tree, "<{:?}>", ParseTag::IfStatement)
            .expect("Failed to write <IfStatement>.");
        self.indent_amount += 2;

        if_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token != Token::OpenParen {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '('.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        if_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        while !(self.current_token().token == Token::CloseParen
            && self.peek().token == Token::OpenCurly)
        {
            if_parse_tree.push_str(&self.parse_expression()?);
        }

        if self.tokens[self.index].token != Token::CloseParen {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected ')'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        if_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token != Token::OpenCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '{'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        if_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if_parse_tree.push_str(&self.parse_statements()?);

        if self.current_token().token != Token::CloseCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '}'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        if_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        // If there is no else, the function ends here. Otherwise it continues
        // to parse the else.
        if self.current_token().token != Token::Else {
            self.indent_amount -= 2;
            if_parse_tree.push_str(&self.generate_indent());
            writeln!(if_parse_tree, "</{:?}>", ParseTag::IfStatement)
                .expect("Failed to write </IfStatement>.");
            return Ok(if_parse_tree);
        }

        if_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if self.tokens[self.index].token != Token::OpenCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '{'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        if_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        if_parse_tree.push_str(&self.parse_statements()?);

        if self.current_token().token != Token::CloseCurly {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '}'.",
                Some(self.tokens[self.index].path.clone()),
                Some(self.tokens[self.index].line),
                Some(self.tokens[self.index].column),
            ));
        }

        if_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        self.indent_amount -= 2;
        if_parse_tree.push_str(&self.generate_indent());
        writeln!(if_parse_tree, "</{:?}>", ParseTag::IfStatement)
            .expect("Failed to write </IfStatement>.");

        Ok(if_parse_tree)
    }

    fn parse_expression(&mut self) -> Result<String, JackError> {
        let mut expression_parse_tree = self.generate_indent();

        writeln!(expression_parse_tree, "<{:?}>", ParseTag::Expression)
            .expect("Failed to write <Expression>.");
        self.indent_amount += 2;

        expression_parse_tree.push_str(&self.parse_term()?);

        while self.current_token().token.is_op() {
            expression_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;
            expression_parse_tree.push_str(&self.parse_term()?);
        }

        self.indent_amount -= 2;
        expression_parse_tree.push_str(&self.generate_indent());
        writeln!(expression_parse_tree, "</{:?}>", ParseTag::Expression)
            .expect("Failed to write </Expression>.");

        Ok(expression_parse_tree)
    }

    fn parse_term(&mut self) -> Result<String, JackError> {
        let mut term_parse_tree = self.generate_indent();

        writeln!(term_parse_tree, "<{:?}>", ParseTag::Term).expect("Failed to write <Term>.");
        self.indent_amount += 2;

        // We should be on the first Token of the term by the time we get to
        // this point of parsing a term.

        if matches!(
            self.current_token().token,
            Token::IntegerConstant | Token::StringConstant
        ) || self.current_token().token.is_keyword_constant()
        {
            term_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;
        } else if self.current_token().token == Token::Identifier {
            let next = &self.peek().token;

            // identifier[expression]
            if *next == Token::OpenBracket {
                // Identifier
                term_parse_tree.push_str(&self.generate_xml_tag());
                self.index += 1;
                // OpenBracket
                term_parse_tree.push_str(&self.generate_xml_tag());
                self.index += 1;
                // Go parse the expression. Should return us back to the Close
                // Bracket.
                term_parse_tree.push_str(&self.parse_expression()?);

                if self.current_token().token != Token::CloseBracket {
                    return Err(JackError::new(
                        ErrorType::UnexpectedToken,
                        "Expected ']'.",
                        Some(self.tokens[self.index].path.clone()),
                        Some(self.tokens[self.index].line),
                        Some(self.tokens[self.index].column),
                    ));
                }

                term_parse_tree.push_str(&self.generate_xml_tag());
                self.index += 1;
            // identifer.identifier(...) || identifier(...)
            } else if *next == Token::Dot || *next == Token::OpenParen {
                term_parse_tree.push_str(&self.parse_subroutine_call()?);
            // identifier
            } else {
                term_parse_tree.push_str(&self.generate_xml_tag());
                self.index += 1;
            }
        } else if self.current_token().token == Token::OpenParen {
            term_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;

            term_parse_tree.push_str(&self.parse_expression()?);

            if self.current_token().token != Token::CloseParen {
                return Err(JackError::new(
                    ErrorType::UnexpectedToken,
                    "Expected ')'.",
                    Some(self.tokens[self.index].path.clone()),
                    Some(self.tokens[self.index].line),
                    Some(self.tokens[self.index].column),
                ));
            }

            term_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;
        } else if matches!(self.current_token().token, Token::Minus | Token::Tilde) {
            term_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;
            term_parse_tree.push_str(&self.parse_term()?);
        }

        self.indent_amount -= 2;
        term_parse_tree.push_str(&self.generate_indent());
        writeln!(term_parse_tree, "</{:?}>", ParseTag::Term).expect("Failed to write </Term>.");

        Ok(term_parse_tree)
    }

    // Expression lists are lists of expressions separated by commas. They CAN
    // be empty.
    fn parse_expression_list(&mut self) -> Result<String, JackError> {
        let mut expression_list_parse_tree = self.generate_indent();

        writeln!(
            expression_list_parse_tree,
            "<{:?}>",
            ParseTag::ExpressionList
        )
        .expect("Failed to write <ExpressionList>.");
        self.indent_amount += 2;

        if self.current_token().token != Token::CloseParen {
            expression_list_parse_tree.push_str(&self.parse_expression()?);
        }

        while self.current_token().token == Token::Comma {
            // Add the comma to the parse tree and advance to start of
            // expression.
            expression_list_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;

            expression_list_parse_tree.push_str(&self.parse_expression()?);
        }

        self.indent_amount -= 2;
        expression_list_parse_tree.push_str(&self.generate_indent());
        writeln!(
            expression_list_parse_tree,
            "</{:?}>",
            ParseTag::ExpressionList
        )
        .expect("Failed to write </ExpressionList>.");

        Ok(expression_list_parse_tree)
    }

    fn parse_subroutine_call(&mut self) -> Result<String, JackError> {
        // Add the identifier to the parse tree. If we call this function we are
        // guaranteed to be on an identifier.
        let mut subroutine_call_parse_tree = self.generate_xml_tag();

        self.index += 1;

        // Will only occur if subroutine is a method of a class.
        if self.current_token().token == Token::Dot {
            subroutine_call_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;

            if self.current_token().token != Token::Identifier {
                return Err(JackError::new(
                    ErrorType::MissingIdentifier,
                    "Expected identifer for class method. Class.identifier(..).",
                    Some(self.current_token().path.clone()),
                    Some(self.current_token().line),
                    Some(self.current_token().column),
                ));
            }

            subroutine_call_parse_tree.push_str(&self.generate_xml_tag());
            self.index += 1;
        }

        if self.current_token().token != Token::OpenParen {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected '(' in subroutine call.",
                Some(self.current_token().path.clone()),
                Some(self.current_token().line),
                Some(self.current_token().column),
            ));
        }

        subroutine_call_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        subroutine_call_parse_tree.push_str(&self.parse_expression_list()?);

        if self.current_token().token != Token::CloseParen {
            return Err(JackError::new(
                ErrorType::UnexpectedToken,
                "Expected ')' in subroutine call.",
                Some(self.current_token().path.clone()),
                Some(self.current_token().line),
                Some(self.current_token().column),
            ));
        }

        subroutine_call_parse_tree.push_str(&self.generate_xml_tag());
        self.index += 1;

        Ok(subroutine_call_parse_tree)
    }

    fn generate_xml_tag(&self) -> String {
        let token = self.current_token();

        let mut xml_tag = String::new();

        xml_tag.push_str(&self.generate_indent());
        write!(xml_tag, "<{:?}>", token.token_type).expect("Failed to write to xml_tag.");
        xml_tag.push_str(
            token
                .token_str
                .as_ref()
                .unwrap_or(&format!("{:?}", token.token))
                .as_str(),
        );
        writeln!(xml_tag, "</{:?}>", token.token_type).expect("Failed to write to xml_tag.");

        xml_tag
    }

    fn generate_indent(&self) -> String {
        " ".repeat(self.indent_amount)
    }
}
