#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum Kind {
    Static,
    Field,
    Arg,
    Local,
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
    pub symbol_table: Vec<Symbol>,
    pub static_index: u32,
    pub field_index: u32,
    pub arg_index: u32,
    pub local_index: u32,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbol_table: Vec::new(),
            static_index: 0,
            field_index: 0,
            arg_index: 0,
            local_index: 0,
        }
    }

    pub fn define(&self, name: String, ty: String, kind: Kind) {
        let index = match kind {
            Kind::Static => self.static_index,
            Kind::Arg => self.field_index,
            Kind::Field => self.arg_index,
            Kind::Local => self.local_index,
        };

        let symbol = Symbol::new(name, ty, kind, index);

        match kind {
            Kind::Static => self.static_index += 1,
            Kind::Arg => self.field_index += 1,
            Kind::Field => self.arg_index += 1,
            Kind::Local => self.local_index += 1,
        }
    }

    pub fn clear_table(&mut self) {
        self.symbol_table.clear();
        self.static_index = 0;
        self.field_index = 0;
        self.arg_index = 0;
        self.local_index = 0;
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
