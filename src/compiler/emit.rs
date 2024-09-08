//! Common methods for emitting codes to load/store values

use crate::{model::StructFieldDef, Type};

use super::{encode_leb128, encode_sleb128, Compiler, OpCode};

impl<'a> Compiler<'a> {
    pub(super) fn i32const(&mut self, val: u32) {
        self.code.push(OpCode::I32Const as u8);
        encode_leb128(&mut self.code, val).unwrap();
    }

    #[allow(dead_code)]
    pub(super) fn i32and(&mut self, val: u32) {
        self.code.push(OpCode::I32And as u8);
        encode_sleb128(&mut self.code, val as i32).unwrap();
    }

    pub(super) fn i32load(&mut self, offset: u32) -> Result<(), String> {
        self.code.push(OpCode::I32Load as u8);
        encode_leb128(&mut self.code, 0).unwrap();
        encode_leb128(&mut self.code, offset).unwrap();
        Ok(())
    }

    pub(super) fn i32load8_s(&mut self, offset: u32) -> Result<(), String> {
        self.code.push(OpCode::I32Load8S as u8);
        encode_leb128(&mut self.code, 0).unwrap();
        encode_leb128(&mut self.code, offset).unwrap();
        Ok(())
    }

    pub(super) fn store(&mut self, op: OpCode, offset: u32) -> Result<(), String> {
        self.code.push(op as u8);
        encode_leb128(&mut self.code, 0).unwrap();
        encode_leb128(&mut self.code, offset).unwrap();
        Ok(())
    }

    pub(super) fn i32store(&mut self, offset: u32) -> Result<(), String> {
        self.store(OpCode::I32Store, offset)
    }

    pub(super) fn i64store(&mut self, offset: u32) -> Result<(), String> {
        self.store(OpCode::I64Store, offset)
    }

    pub(super) fn f32store(&mut self, offset: u32) -> Result<(), String> {
        self.store(OpCode::F32Store, offset)
    }

    pub(super) fn f64store(&mut self, offset: u32) -> Result<(), String> {
        self.store(OpCode::F64Store, offset)
    }

    pub(super) fn i32store8(&mut self, offset: u32) -> Result<(), String> {
        self.store(OpCode::I32Store8, offset)
    }

    pub(super) fn field_load(&mut self, field: &StructFieldDef) -> Result<Type, String> {
        self.code.push(match field.ty {
            Type::I32 | Type::Str | Type::Struct(_) => OpCode::I32Load,
            Type::I64 => OpCode::I64Load,
            Type::F32 => OpCode::F32Load,
            Type::F64 => OpCode::F64Load,
            Type::Void => return Ok(Type::Void),
        } as u8);
        encode_leb128(&mut self.code, 0).unwrap();
        encode_leb128(&mut self.code, field.offset as u32).unwrap();
        Ok(field.ty.clone())
    }
}
