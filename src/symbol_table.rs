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
}

impl Symbol {
    pub fn new(name: String, ty: String, kind: Kind) -> Self {
        Self { name, ty, kind }
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    pub symbol_table: Vec<Symbol>,
    pub index: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbol_table: Vec::new(),
            index: 0,
        }
    }

    pub fn define(&self, name: String, ty: String, kind: Kind) {
        let symbol = Symbol::new(name, ty, kind);
        self.index += 1;
    }

    pub fn clear_table(&mut self) {
        self.symbol_table.clear();
        self.index = 0;
    }

    pub fn var_count(&self, kind: Kind) -> u32 {
        let mut count: u32 = 0;
        for symbol in &self.symbol_table {
            if symbol.kind == kind {
                count += 1;
            }
        }
        count
    }

    pub fn kind_of(&self, name_needle: String) -> Option<Kind> {
        let mut kind: Option<Kind> = None;
        for symbol in &self.symbol_table {
            if symbol.name == name_needle {
                kind = Some(symbol.kind);
            }
        }
        kind
    }

    pub fn type_of(&self, name_needle: String) -> Option<String> {
        let mut ty: Option<String> = None;
        for symbol in &self.symbol_table {
            if symbol.ty == name_needle {
                ty = Some(symbol.ty.clone());
            }
        }
        ty
    }

    // Returns an option in case the name is not found.
    pub fn index_of(&self, name_needle: String) -> Option<u32> {
        let mut index: Option<u32> = None;
        for symbol in &self.symbol_table {
            if symbol.ty == name_needle {
                index = Some(self.index);
            }
        }
        index
    }
}
