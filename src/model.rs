use crate::parser::VarDecl;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    // Pseudo type representing no value
    Void,
    Str,
}

impl Type {
    pub(crate) fn code(&self) -> u8 {
        match self {
            Self::I32 => 0x7f,
            Self::I64 => 0x7e,
            Self::F32 => 0x7d,
            Self::F64 => 0x7c,
            Self::Void => 0x40,
            // Str is a compound type, but it is returned as an i32 pointing the buffer.
            Self::Str => 0x7f,
        }
    }

    /// Returns the number of words to represent this value.
    /// Primitive types typically have a single word, while most variable-length type
    /// has 2 words (a pointer and a size), and Void type has 0 words.
    pub(crate) fn word_count(&self) -> usize {
        match self {
            Self::Str | Self::I32 | Self::I64 | Self::F32 | Self::F64 => 1,
            Self::Void => 0,
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
            "void" => Type::Void,
            "str" => Type::Str,
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
            Self::Str => write!(f, "str"),
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct TypeSet {
    pub i32: bool,
    pub i64: bool,
    pub f32: bool,
    pub f64: bool,
    pub void: bool,
    pub st: bool,
}

impl TypeSet {
    pub const I32: Self = Self {
        i32: true,
        i64: false,
        f32: false,
        f64: false,
        void: false,
        st: false,
    };
    pub const I64: Self = Self {
        i32: false,
        i64: true,
        f32: false,
        f64: false,
        void: false,
        st: false,
    };
    pub const F32: Self = Self {
        i32: false,
        i64: false,
        f32: true,
        f64: false,
        void: false,
        st: false,
    };
    pub const F64: Self = Self {
        i32: false,
        i64: false,
        f32: false,
        f64: true,
        void: false,
        st: false,
    };
    pub const VOID: Self = Self {
        i32: false,
        i64: false,
        f32: false,
        f64: false,
        void: true,
        st: false,
    };
    pub const STR: Self = Self {
        i32: false,
        i64: false,
        f32: false,
        f64: false,
        void: false,
        st: true,
    };
    pub const ALL: Self = Self {
        i32: true,
        i64: true,
        f32: true,
        f64: true,
        void: true,
        st: true,
    };

    pub fn is_none(&self) -> bool {
        !self.i32 && !self.i64 && !self.f32 && !self.f64 && !self.st
    }

    pub fn determine(&self) -> Option<Type> {
        match self {
            &Self::I32 => Some(Type::I32),
            &Self::I64 => Some(Type::I64),
            &Self::F32 => Some(Type::F32),
            &Self::F64 => Some(Type::F64),
            &Self::VOID => Some(Type::Void),
            &Self::STR => Some(Type::Str),
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
            void: self.void | rhs.void,
            st: self.st | rhs.st,
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
            void: self.void & rhs.void,
            st: self.st & rhs.st,
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
            Type::Void => ret.void = true,
            Type::Str => ret.st = true,
        }
        ret
    }
}

impl std::fmt::Display for TypeSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.i32 & self.i64 & self.f32 & self.f64 & self.void & self.st {
            return write!(f, "any");
        }
        let mut written = false;
        let mut write_ty = |val, name| {
            if val {
                if written {
                    write!(f, "|")?;
                }
                write!(f, "{name}")?;
                written = true;
            }
            Ok(())
        };
        write_ty(self.i32, "i32")?;
        write_ty(self.i64, "i64")?;
        write_ty(self.f32, "f32")?;
        write_ty(self.f64, "f64")?;
        write_ty(self.void, "void")?;
        write_ty(self.st, "str")?;
        if !written {
            write!(f, "(none)")?;
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
