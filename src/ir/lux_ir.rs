//! Lux IR definitions.
//!
//! A lower-level representation closer to machine operations.

/// A Lux IR module (compilation unit).
#[derive(Debug, Clone)]
pub struct IrModule {
    pub functions: Vec<IrFunction>,
    pub structs: Vec<IrStruct>,
}

/// An IR function.
#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<(String, IrType)>,
    pub return_type: IrType,
    pub blocks: Vec<IrBlock>,
}

/// A basic block in the IR.
#[derive(Debug, Clone)]
pub struct IrBlock {
    pub label: String,
    pub instructions: Vec<IrInst>,
    pub terminator: IrTerminator,
}

/// IR instructions.
#[derive(Debug, Clone)]
pub enum IrInst {
    /// Allocate stack space
    Alloca { dest: String, ty: IrType },
    /// Store value to memory
    Store { value: IrValue, ptr: String },
    /// Load value from memory
    Load {
        dest: String,
        ptr: String,
        ty: IrType,
    },
    /// Binary operation
    BinOp {
        dest: String,
        op: IrBinOp,
        left: IrValue,
        right: IrValue,
    },
    /// Function call
    Call {
        dest: Option<String>,
        func: String,
        args: Vec<IrValue>,
    },
    /// Get element pointer
    GetElementPtr {
        dest: String,
        ptr: String,
        indices: Vec<IrValue>,
    },
}

/// Block terminators.
#[derive(Debug, Clone)]
pub enum IrTerminator {
    /// Return from function
    Return(Option<IrValue>),
    /// Unconditional branch
    Branch(String),
    /// Conditional branch
    CondBranch {
        cond: IrValue,
        true_block: String,
        false_block: String,
    },
    /// Unreachable (for error paths)
    Unreachable,
}

/// IR values.
#[derive(Debug, Clone)]
pub enum IrValue {
    /// Integer constant
    ConstInt(i64, IrType),
    /// Float constant
    ConstFloat(f64, IrType),
    /// Boolean constant
    ConstBool(bool),
    /// Variable reference
    Var(String),
    /// Unit value
    Unit,
}

/// Binary operations in IR.
#[derive(Debug, Clone, Copy)]
pub enum IrBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

/// IR types.
#[derive(Debug, Clone, PartialEq)]
pub enum IrType {
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Bool,
    Ptr(Box<IrType>),
    Array(Box<IrType>, usize),
    Struct(String),
    Void,
}

/// An IR struct definition.
#[derive(Debug, Clone)]
pub struct IrStruct {
    pub name: String,
    pub fields: Vec<(String, IrType)>,
}
