use crate::parser::VarDecl;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    // Pseudo type representing no value
    Void,
}

impl Type {
    pub(crate) fn code(&self) -> u8 {
        match self {
            Self::I32 => 0x7f,
            Self::I64 => 0x7e,
            Self::F32 => 0x7d,
            Self::F64 => 0x7c,
            Self::Void => 0x40,
        }
    }
}

impl From<u8> for Type {
    fn from(value: u8) -> Self {
        match value {
            0x7f => Self::I32,
            0x7e => Self::I64,
            0x7d => Self::F32,
            0x7c => Self::F64,
            0x40 => Self::Void,
            _ => panic!("Unknown type"),
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => return Err(format!("Unknown type {}", value)),
        })
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
            Self::Void => write!(f, "void"),
        }
    }
}

pub struct FuncType {
    pub params: Vec<Type>,
    pub results: Vec<Type>,
}

pub struct FuncImport {
    pub module: String,
    pub name: String,
    pub ty: usize,
}

pub(crate) struct FuncDef {
    pub name: String,
    pub ty: usize,
    pub code: Vec<u8>,
    pub locals: Vec<VarDecl>,
}
