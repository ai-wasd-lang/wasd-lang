//! Type representations for the WASD type system.

/// Internal type representation used during type checking.
#[derive(Debug, Clone, PartialEq)]
pub enum WasdType {
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
    Generic(String, Vec<WasdType>),
    /// Reference type
    Ref(Box<WasdType>, bool), // (inner, is_mut)
    /// Heap-allocated type
    Heap(Box<WasdType>),
    /// Reference-counted type
    Rc(Box<WasdType>),
    /// Atomically reference-counted type
    Arc(Box<WasdType>),
    /// Function type
    Function {
        params: Vec<WasdType>,
        ret: Box<WasdType>,
        effects: Vec<String>,
    },
    /// Type variable for inference
    Var(usize),
    /// Unknown type (error recovery)
    Unknown,
}

impl WasdType {
    /// Check if this type is a primitive type.
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            WasdType::I8
                | WasdType::I16
                | WasdType::I32
                | WasdType::I64
                | WasdType::U8
                | WasdType::U16
                | WasdType::U32
                | WasdType::U64
                | WasdType::F32
                | WasdType::F64
                | WasdType::Bool
                | WasdType::String
                | WasdType::Unit
        )
    }

    /// Check if this type is copyable (doesn't need move semantics).
    pub fn is_copy(&self) -> bool {
        matches!(
            self,
            WasdType::I8
                | WasdType::I16
                | WasdType::I32
                | WasdType::I64
                | WasdType::U8
                | WasdType::U16
                | WasdType::U32
                | WasdType::U64
                | WasdType::F32
                | WasdType::F64
                | WasdType::Bool
        )
    }
}
