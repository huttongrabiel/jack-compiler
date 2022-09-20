use ansi_term::Color::{Red, RGB};
use std::fmt::Display;

#[derive(Debug)]
pub enum ErrorType {
    GarbageToken,
    UnclosedQuotation,
    IOError,
    InvalidInteger,
    GeneralError,
    MissingIdentifier,
    BadMultiVariableDeclaration,
    BadParameterList,
    MissingType,
    UnexpectedToken,
    UndeclaredVariable,
    ExpectedSemicolon,
    NoReturn,
    NoClassDeclaration,
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::GarbageToken => write!(f, "GarbageToken"),
            ErrorType::UnclosedQuotation => write!(f, "UnclosedQuotation"),
            ErrorType::IOError => write!(f, "IOError"),
            ErrorType::InvalidInteger => write!(f, "InvalidInteger"),
            ErrorType::GeneralError => write!(f, "GeneralError"),
            ErrorType::MissingIdentifier => write!(f, "MissingIdentifier"),
            ErrorType::BadMultiVariableDeclaration => write!(f, "BadMultiVariableDeclaration"),
            ErrorType::BadParameterList => write!(f, "BadParameterList"),
            ErrorType::MissingType => write!(f, "MissingType"),
            ErrorType::UnexpectedToken => write!(f, "UnexpectedToken"),
            ErrorType::UndeclaredVariable => write!(f, "UndeclaredVariable"),
            ErrorType::ExpectedSemicolon => write!(f, "ExpectedSemicolon"),
            ErrorType::NoReturn => write!(f, "NoReturn"),
            ErrorType::NoClassDeclaration => write!(f, "NoClassDeclaration"),
        }
    }
}

#[derive(Debug)]
pub struct JackError {
    error_type: ErrorType,
    message: &'static str,
    // file, line, and column are options because we can error while not
    // operating on a file. Such as not providing a file/directory at all.
    file: Option<String>,
    line: Option<u64>,
    column: Option<u16>,
}

impl Display for JackError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let orange = RGB(235, 140, 52);
        let pink = RGB(230, 108, 201);

        if self.file.is_some() && self.line.is_some() && self.column.is_some() {
            write!(
                f,
                "{} {}\n\t{}{}:{}:{}\n\t{}",
                Red.paint("Error:"),
                self.error_type,
                orange.paint("file->"),
                self.file.as_ref().unwrap(),
                self.line.unwrap(),
                self.column.unwrap(),
                pink.paint(self.message),
            )
        } else {
            write!(
                f,
                "{} {}\n\t{}",
                Red.paint("Error:"),
                self.error_type,
                pink.paint(self.message),
            )
        }
    }
}

impl JackError {
    pub fn new(
        error_type: ErrorType,
        message: &'static str,
        file: Option<String>,
        line: Option<u64>,
        column: Option<u16>,
    ) -> Self {
        Self {
            error_type,
            message,
            file,
            line,
            column,
        }
    }
}
