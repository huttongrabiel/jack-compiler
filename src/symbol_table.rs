#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Kind {
    Static,
    Field,
    Arg,
    Var,
}

#[derive(Debug)]
pub struct Symbol {
    name: String,
    ty: String,
    kind: Kind,
    index: u32,
}

impl Symbol {
    pub fn new(name: String, ty: String, kind: Kind, index: u32) -> Self {
        Self {
            name,
            ty,
            kind,
            index,
        }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    symbol_table: Vec<Symbol>,
    index: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbol_table: Vec::new(),
            index: 0,
        }
    }

    fn clear_table(&mut self) {
        self.symbol_table.clear();
        self.index = 0;
    }

    fn var_count(&self, kind: Kind) -> u32 {
        let mut count: u32 = 0;
        for symbol in &self.symbol_table {
            if symbol.kind == kind {
                count += 1;
            }
        }
        count
    }

    fn kind_of(&self, name_needle: String) -> Option<Kind> {
        let mut kind: Option<Kind> = None;
        for symbol in &self.symbol_table {
            if symbol.name == name_needle {
                kind = Some(symbol.kind);
            }
        }
        kind
    }

    fn type_of(&self, name_needle: String) -> Option<String> {
        let mut ty: Option<String> = None;
        for symbol in &self.symbol_table {
            if symbol.ty == name_needle {
                ty = Some(symbol.ty.clone());
            }
        }
        ty
    }

    fn index_of(&self, name_needle: String) -> Option<u32> {
        let mut index: Option<u32> = None;
        for symbol in &self.symbol_table {
            if symbol.ty == name_needle {
                index = Some(symbol.index);
            }
        }
        index
    }
}
