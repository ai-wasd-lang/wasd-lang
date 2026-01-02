//! Type representations for the Lux type system.

/// Internal type representation used during type checking.
#[derive(Debug, Clone, PartialEq)]
pub enum LuxType {
    /// Primitive integer types
    I8,
    I16,
    I32,
    I64,
    /// Primitive unsigned integer types
    U8,
    U16,
    U32,
    U64,
    /// Floating point types
    F32,
    F64,
    /// Boolean type
    Bool,
    /// String type
    String,
    /// Unit type (no value)
    Unit,
    /// Named user-defined type
    Named(String),
    /// Generic type with parameters
    Generic(String, Vec<LuxType>),
    /// Reference type
    Ref(Box<LuxType>, bool), // (inner, is_mut)
    /// Heap-allocated type
    Heap(Box<LuxType>),
    /// Reference-counted type
    Rc(Box<LuxType>),
    /// Atomically reference-counted type
    Arc(Box<LuxType>),
    /// Function type
    Function {
        params: Vec<LuxType>,
        ret: Box<LuxType>,
        effects: Vec<String>,
    },
    /// Type variable for inference
    Var(usize),
    /// Unknown type (error recovery)
    Unknown,
}

impl LuxType {
    /// Check if this type is a primitive type.
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            LuxType::I8
                | LuxType::I16
                | LuxType::I32
                | LuxType::I64
                | LuxType::U8
                | LuxType::U16
                | LuxType::U32
                | LuxType::U64
                | LuxType::F32
                | LuxType::F64
                | LuxType::Bool
                | LuxType::String
                | LuxType::Unit
        )
    }

    /// Check if this type is copyable (doesn't need move semantics).
    pub fn is_copy(&self) -> bool {
        matches!(
            self,
            LuxType::I8
                | LuxType::I16
                | LuxType::I32
                | LuxType::I64
                | LuxType::U8
                | LuxType::U16
                | LuxType::U32
                | LuxType::U64
                | LuxType::F32
                | LuxType::F64
                | LuxType::Bool
        )
    }
}
