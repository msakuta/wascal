use std::collections::HashSet;

use crate::parser::VarDecl;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    // Pseudo type representing no value
    Void,
    Str,
    Struct(String),
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
            // Struct is also heap allocated.
            Self::Struct(_) => 0x7f,
        }
    }

    /// Returns the number of words to represent this value.
    /// Primitive types typically have a single word, while most variable-length type
    /// has 2 words (a pointer and a size), and Void type has 0 words.
    pub(crate) fn word_count(&self) -> usize {
        match self {
            Self::Str | Self::I32 | Self::I64 | Self::F32 | Self::F64 | Self::Struct(_) => 1,
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
            s => {
                if s.is_empty() {
                    return Err(format!("Unknown type {}", value));
                } else {
                    Type::Struct(s.to_string())
                }
            }
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
            Self::Struct(name) => write!(f, "{name}"),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct TypeSet {
    pub i32: bool,
    pub i64: bool,
    pub f32: bool,
    pub f64: bool,
    pub void: bool,
    pub st: bool,
    pub structs: HashSet<String>,
}

impl TypeSet {
    pub fn i32() -> Self {
        Self {
            i32: true,
            i64: false,
            f32: false,
            f64: false,
            void: false,
            st: false,
            structs: HashSet::new(),
        }
    }

    pub fn i64() -> Self {
        Self {
            i32: false,
            i64: true,
            f32: false,
            f64: false,
            void: false,
            st: false,
            structs: HashSet::new(),
        }
    }

    pub fn f32() -> Self {
        Self {
            i32: false,
            i64: false,
            f32: true,
            f64: false,
            void: false,
            st: false,
            structs: HashSet::new(),
        }
    }

    pub fn f64() -> Self {
        Self {
            i32: false,
            i64: false,
            f32: false,
            f64: true,
            void: false,
            st: false,
            structs: HashSet::new(),
        }
    }

    pub fn void() -> Self {
        Self {
            i32: false,
            i64: false,
            f32: false,
            f64: false,
            void: true,
            st: false,
            structs: HashSet::new(),
        }
    }

    pub fn str() -> Self {
        Self {
            i32: false,
            i64: false,
            f32: false,
            f64: false,
            void: false,
            st: true,
            structs: HashSet::new(),
        }
    }

    pub fn all() -> Self {
        Self {
            i32: true,
            i64: true,
            f32: true,
            f64: true,
            void: true,
            st: true,
            structs: HashSet::new(),
        }
    }

    pub fn is_non_primitive(&self) -> bool {
        !self.i32 && !self.i64 && !self.f32 && !self.f64 && !self.st
    }

    pub fn is_none(&self) -> bool {
        self.is_non_primitive() && self.structs.is_empty()
    }

    pub fn iter_primitives(&self) -> impl Iterator<Item = (Type, bool)> {
        std::iter::once((Type::I32, self.i32))
            .chain(std::iter::once((Type::I64, self.i64)))
            .chain(std::iter::once((Type::F32, self.f32)))
            .chain(std::iter::once((Type::F64, self.f64)))
            .chain(std::iter::once((Type::Void, self.void)))
    }

    pub fn determine(&self) -> Option<Type> {
        if self.structs.is_empty() {
            if self == &Type::I32.into() {
                return Some(Type::I32);
            } else if self == &Type::I64.into() {
                return Some(Type::I64);
            } else if self == &Type::F32.into() {
                return Some(Type::F32);
            } else if self == &Type::F64.into() {
                return Some(Type::F64);
            } else if self == &Type::Void.into() {
                return Some(Type::Void);
            } else if self == &Type::Str.into() {
                return Some(Type::Str);
            }
            None
        } else if self.is_non_primitive() && self.structs.len() == 1 {
            self.structs
                .iter()
                .next()
                .map(|name| Type::Struct(name.clone()))
        } else {
            None
        }
    }
}

impl From<u8> for TypeSet {
    fn from(value: u8) -> Self {
        Self {
            i32: value & 0x1 != 0,
            i64: value & 0x2 != 0,
            f32: value & 0x4 != 0,
            f64: value & 0x8 != 0,
            void: value & 0x10 != 0,
            st: value & 0x20 != 0,
            structs: HashSet::new(),
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
            structs: self.structs.union(&rhs.structs).cloned().collect(),
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
            structs: self.structs.intersection(&rhs.structs).cloned().collect(),
        }
    }
}

impl std::ops::BitAnd for &TypeSet {
    type Output = TypeSet;
    fn bitand(self, rhs: Self) -> Self::Output {
        TypeSet {
            i32: self.i32 & rhs.i32,
            i64: self.i64 & rhs.i64,
            f32: self.f32 & rhs.f32,
            f64: self.f64 & rhs.f64,
            void: self.void & rhs.void,
            st: self.st & rhs.st,
            structs: self.structs.intersection(&rhs.structs).cloned().collect(),
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
            Type::Struct(name) => {
                ret.structs.insert(name);
            }
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

        for st in &self.structs {
            write_ty(true, st)?;
        }

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
    /// The number of arguments as first n locals
    pub args: usize,
    pub ret_ty: Type,
    pub code: Vec<u8>,
    pub locals: Vec<VarDecl>,
    pub public: bool,
}
