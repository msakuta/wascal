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

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct TypeSet {
    pub i32: bool,
    pub i64: bool,
    pub f32: bool,
    pub f64: bool,
}

impl TypeSet {
    pub const I32: Self = Self {
        i32: true,
        i64: false,
        f32: false,
        f64: false,
    };
    pub const I64: Self = Self {
        i32: false,
        i64: true,
        f32: false,
        f64: false,
    };
    pub const F32: Self = Self {
        i32: false,
        i64: false,
        f32: true,
        f64: false,
    };
    pub const F64: Self = Self {
        i32: false,
        i64: false,
        f32: false,
        f64: true,
    };
    pub const ALL: Self = Self {
        i32: true,
        i64: true,
        f32: true,
        f64: true,
    };

    pub fn is_none(&self) -> bool {
        !self.i32 && !self.i64 && !self.f32 && !self.f64
    }

    pub fn determine(&self) -> Option<Type> {
        match self {
            TypeSet {
                i32: true,
                i64: false,
                f32: false,
                f64: false,
            } => Some(Type::I32),
            TypeSet {
                i32: false,
                i64: true,
                f32: false,
                f64: false,
            } => Some(Type::I64),
            TypeSet {
                i32: false,
                i64: false,
                f32: true,
                f64: false,
            } => Some(Type::F32),
            TypeSet {
                i32: false,
                i64: false,
                f32: false,
                f64: true,
            } => Some(Type::F64),
            _ => None,
        }
    }
}

impl std::ops::BitOr for TypeSet {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            i32: self.i32 | rhs.i32,
            i64: self.i64 | rhs.i64,
            f32: self.f32 | rhs.f32,
            f64: self.f64 | rhs.f64,
        }
    }
}

impl std::ops::BitAnd for TypeSet {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Self {
            i32: self.i32 & rhs.i32,
            i64: self.i64 & rhs.i64,
            f32: self.f32 & rhs.f32,
            f64: self.f64 & rhs.f64,
        }
    }
}

impl From<Type> for TypeSet {
    fn from(value: Type) -> Self {
        let mut ret = Self::default();
        match value {
            Type::I32 => ret.i32 = true,
            Type::I64 => ret.i64 = true,
            Type::F32 => ret.f32 = true,
            Type::F64 => ret.f64 = true,
            _ => {}
        }
        ret
    }
}

impl std::fmt::Display for TypeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut written = false;
        if self.i32 {
            write!(f, "i32")?;
            written = true;
        }
        if self.i64 {
            if written {
                write!(f, "|")?;
            }
            write!(f, "i64")?;
            written = true;
        }
        if self.f32 {
            if written {
                write!(f, "|")?;
            }
            write!(f, "f32")?;
            written = true;
        }
        if self.f64 {
            if written {
                write!(f, "|")?;
            }
            write!(f, "f64")?;
            written = true;
        }
        if !written {
            write!(f, "void")?;
        }
        Ok(())
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
    pub public: bool,
}
