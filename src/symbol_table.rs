use crate::{
    error::{ErrorType, JackError},
    syntax_analyzer::FileData,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Kind {
    Static,
    Field,
    Arg,
    Var,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Scope {
    Class,
    Subroutine,
}

#[derive(Debug)]
pub struct Symbol {
    identifier: String,
    ty: String,
    kind: Kind,
    scope: Scope,
    pos: FileData,
    index: u32,
}

impl Symbol {
    pub fn new(
        identifier: String,
        ty: String,
        kind: Kind,
        scope: Scope,
        pos: FileData,
        index: u32,
    ) -> Self {
        Self {
            identifier,
            ty,
            kind,
            scope,
            pos,
            index,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub class_symbol_table: Vec<Symbol>,
    class_index: u32,
    pub subroutine_symbol_table: Vec<Symbol>,
    subroutine_index: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            class_symbol_table: Vec::new(),
            subroutine_symbol_table: Vec::new(),
            class_index: 0,
            subroutine_index: 0,
        }
    }

    pub fn insert(&mut self) -> Result<(), JackError> {
        Ok(())
    }

    pub fn clear_subroutine_table(&mut self) -> Result<(), JackError> {
        self.subroutine_symbol_table.clear();
        Ok(())
    }
}
