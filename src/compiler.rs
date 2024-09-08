mod emit;
mod malloc;
mod set;
mod strcat;

use std::{
    collections::HashMap,
    io::{Read, Write},
};

use crate::{
    const_table::ConstTable,
    model::{FuncDef, FuncImport, FuncType, StructDef, Type},
    parser::{Expression, For, Statement, VarDecl},
};

#[derive(Debug, Clone, Copy)]
pub(crate) enum OpCode {
    Block = 0x02,
    Loop = 0x03,
    If = 0x04,
    Else = 0x05,
    End = 0x0b,
    Br = 0x0c,
    BrIf = 0x0d,
    Return = 0x0F,
    Call = 0x10,
    Drop = 0x1a,
    LocalGet = 0x20,
    LocalSet = 0x21,
    I32Load = 0x28,
    I64Load = 0x29,
    F32Load = 0x2a,
    F64Load = 0x2b,
    I32Load8S = 0x2c,
    I32Store = 0x36,
    I64Store = 0x37,
    F32Store = 0x38,
    F64Store = 0x39,
    I32Store8 = 0x3a,
    I64Store8 = 0x3c,
    I32Const = 0x41,
    I64Const = 0x42,
    F32Const = 0x43,
    F64Const = 0x44,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4a,
    I32GtU = 0x4b,
    I32LeS = 0x4c,
    I32LeU = 0x4d,
    I32GeS = 0x4e,
    I32GeU = 0x4f,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    I64LeS = 0x57,
    I64LeU = 0x58,
    I64GeS = 0x59,
    I64GeU = 0x5a,
    F32Lt = 0x5d,
    F32Gt = 0x5e,
    F32Le = 0x5f,
    F32Ge = 0x60,
    F64Lt = 0x63,
    F64Gt = 0x64,
    F64Le = 0x65,
    F64Ge = 0x66,
    I32Add = 0x6a,
    I32Sub = 0x6b,
    I32Mul = 0x6c,
    I32DivS = 0x6d,
    I32And = 0x71,
    I64Add = 0x7c,
    I64Sub = 0x7d,
    I64Mul = 0x7e,
    I64DivS = 0x7f,
    F32Neg = 0x8c,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    F64Neg = 0x9a,
    F64Add = 0xa0,
    F64Sub = 0xa1,
    F64Mul = 0xa2,
    F64Div = 0xa3,
    I32WrapI64 = 0xa7,
    I32TruncF32S = 0xa8,
    I32TruncF64S = 0xaa,
    I64ExtendI32S = 0xac,
    I64TruncF32S = 0xae,
    I64TruncF64S = 0xb0,
    F32ConvertI32S = 0xb2,
    F32ConvertI64S = 0xb4,
    F32DemoteF64 = 0xb6,
    F64ConvertI32S = 0xb7,
    F64ConvertI64S = 0xb9,
    F64PromoteF32 = 0xbb,
}

macro_rules! impl_op_from {
    ($($op:ident : $nam:literal,)*) => {
        impl From<u8> for OpCode {
            #[allow(non_upper_case_globals)]
            fn from(o: u8) -> Self {
                $(const $op: u8 = OpCode::$op as u8;)*

                match o {
                    $($op => Self::$op,)*
                    _ => panic!("Opcode \"{:02X}\" unrecognized!", o),
                }
            }
        }

        impl OpCode {
            fn to_name(&self) -> &str {
                use OpCode::*;
                match self {
                    $($op => $nam,)*
                }
            }
        }
    }
}

impl_op_from!(
    Block: "block", Loop: "loop", If: "if", Else: "else", End: "end", Br: "br", BrIf: "br_if",
    Return: "return", Call: "call", Drop: "drop",
    LocalGet: "local.get", LocalSet: "local.set",
    I32Load: "i32.load",
    I64Load: "i64.load",
    F32Load: "f32.load",
    F64Load: "f64.load",
    I32Load8S: "i32.load8_s",
    I32Store: "i32.store",
    I64Store: "i64.store",
    F32Store: "f32.store",
    F64Store: "f64.store",
    I32Store8: "i32.store8",
    I64Store8: "i64.store8",
    I32Const: "i32.const",
    I64Const: "i64.const",
    F32Const: "f32.const",
    F64Const: "f64.const",
    I32LtS: "i32.lt_s",
    I32LtU: "i32.lt_u",
    I32GtS: "i32.gt_s",
    I32GtU: "i32.gt_u",
    I32LeS: "i32.le_s",
    I32LeU: "i32.le_u",
    I32GeS: "i32.ge_s",
    I32GeU: "i32.ge_u",
    I64LtS: "i64.lt_s",
    I64LtU: "i64.lt_u",
    I64GtS: "i64.gt_s",
    I64GtU: "i64.gt_u",
    I64LeS: "i64.le_s",
    I64LeU: "i64.le_u",
    I64GeS: "i64.ge_s",
    I64GeU: "i64.ge_u",
    F32Lt: "f32.lt",
    F32Gt: "f32.gt",
    F32Le: "f32.le",
    F32Ge: "f32.ge",
    F64Lt: "f64.lt",
    F64Gt: "f64.gt",
    F64Le: "f64.le",
    F64Ge: "f64.ge",
    I32Add: "i32.add",
    I32Sub: "i32.sub",
    I32Mul: "i32.mul",
    I32DivS: "i32.div_s",
    I32And: "i32.and",
    I64Add: "i64.add",
    I64Sub: "i64.sub",
    I64Mul: "i64.mul",
    I64DivS: "i64.div_s",
    F32Neg: "f32.neg", F32Add: "f32.add", F32Sub: "f32.sub", F32Mul: "f32.mul", F32Div: "f32.div",
    F64Neg: "f64.neg", F64Add: "f64.add", F64Sub: "f64.sub", F64Mul: "f64.mul", F64Div: "f64.div",
    I32WrapI64: "i32.wrap_i64",
    I32TruncF32S: "i32.trunc_f32_s",
    I32TruncF64S: "i32.trunc_f32_s",
    I64ExtendI32S: "i64.extend_i32_s",
    I64TruncF32S: "i64.trunc_f32_s",
    I64TruncF64S: "i64.trunc_f64_s",
    F32ConvertI32S: "f32.convert_i32_s",
    F32ConvertI64S: "f32.convert_i64_s",
    F32DemoteF64: "f32.demote_f64",
    F64ConvertI32S: "f64.convert_i32_s",
    F64ConvertI64S: "f64.convert_i64_s",
    F64PromoteF32: "f64.promote_f32",
);

struct TypeMap {
    i32: OpCode,
    i64: OpCode,
    f32: OpCode,
    f64: OpCode,
    st: Option<Box<dyn Fn(&mut Compiler) -> Result<(), String>>>,
}

const LT_TYPE_MAP: TypeMap = TypeMap {
    i32: OpCode::I32LtS,
    i64: OpCode::I64LtS,
    f32: OpCode::F32Lt,
    f64: OpCode::F64Lt,
    st: None,
};

#[derive(Clone, Copy, Debug)]
enum LValue {
    Local(usize),
    Memory,
}

/// A environment for compiling a function. Note that a program is made of multiple functions,
/// so you need multiple instances of this object.
pub struct Compiler<'a> {
    code: Vec<u8>,
    locals: Vec<VarDecl>,
    ret_ty: Type,
    types: &'a mut Vec<FuncType>,
    imports: &'a [FuncImport],
    const_table: &'a mut ConstTable,
    /// References to other functions to call and type check.
    funcs: &'a mut Vec<FuncDef>,
    structs: &'a HashMap<String, StructDef>,
}

impl<'a> Compiler<'a> {
    pub fn new(
        args: Vec<VarDecl>,
        ret_ty: Type,
        types: &'a mut Vec<FuncType>,
        imports: &'a [FuncImport],
        const_table: &'a mut ConstTable,
        funcs: &'a mut Vec<FuncDef>,
        structs: &'a HashMap<String, StructDef>,
    ) -> Self {
        let locals = args;
        Self {
            code: vec![],
            locals,
            ret_ty,
            types,
            imports,
            const_table,
            funcs,
            structs,
        }
    }

    pub fn compile(&mut self, ast: &[Statement], ty: Type) -> Result<Type, String> {
        // self.i32const(0);
        // self.code.push(OpCode::I32Const as u8);
        // encode_leb128(&mut self.code, 0).unwrap();
        // self.i32load()?;
        // self.code.push(OpCode::I32Const as u8);
        // encode_leb128(&mut self.code, 1).unwrap();
        // self.code.push(OpCode::I32Add as u8);
        // self.i32store()?;

        // self.code.push(OpCode::LocalGet as u8);
        // encode_leb128(&mut self.code, 0).unwrap();

        let last = self.emit_stmts(ast, &ty)?;

        self.code.push(OpCode::End as u8);
        Ok(last)
    }

    fn find_func(&self, name: &str) -> Result<(usize, Type), String> {
        let (idx, ret) = self
            .funcs
            .iter()
            .enumerate()
            .find(|(_, func)| func.name == name)
            .ok_or_else(|| format!("{name} not found"))?;
        Ok((
            idx + self.imports.len(),
            self.types[ret.ty]
                .results
                .get(0)
                .cloned()
                .unwrap_or(Type::Void),
        ))
    }

    /// Find and call a function with a name
    fn call_func(&mut self, name: &str) -> Result<Type, String> {
        let (func_id, ret_ty) = self.find_func(name)?;
        self.code.push(OpCode::Call as u8);
        encode_leb128(&mut self.code, func_id as u32).map_err(|e| e.to_string())?;
        Ok(ret_ty)
    }

    fn define_reverse(&mut self) -> Result<Type, String> {
        // get length of the string
        self.local_get(1);

        let (malloc_id, _) = self.find_func("malloc")?;

        // Allocate another chunk of memory with the same length
        // let new_buf = self.bump()?;
        self.code.push(OpCode::Call as u8);
        encode_leb128(&mut self.code, malloc_id as u32).unwrap();
        let new_buf = self.add_local("", Type::I32);

        self.i32const(0);
        let idx = self.add_local("", Type::I32);

        self.emit_for_loop(idx, 1, Type::I32, |this| {
            this.local_get(idx); // [idx]
            this.local_get(0); // [idx, ptr]

            this.code.push(OpCode::I32Add as u8); // [idx + ptr]

            this.i32load8_s(4)?; // [mem[idx + ptr]]
            let data = this.add_local("", Type::I32); // []

            this.local_get(1); // [len]
            this.local_get(idx); // [len, idx]
            this.code.push(OpCode::I32Sub as u8); // [len - idx]
            this.i32const(1); // [len - idx, 1]
            this.code.push(OpCode::I32Sub as u8); // [len - idx - 1]
            this.local_get(new_buf); // [len - idx - 1, new_buf]
            this.code.push(OpCode::I32Add as u8); // [len - idx - 1 + new_buf]
            this.local_get(data); // [len - idx + new_buf, mem[idx + ptr]]

            this.i32store8(4)?; // []

            Ok(())
        })?;

        // Allocate buffer to return string
        self.i32const(8);
        self.code.push(OpCode::Call as u8);
        encode_leb128(&mut self.code, malloc_id as u32).unwrap();
        let ret_buf = self.codegen_malloc()?;

        // Write ptr
        self.local_get(ret_buf);
        self.local_get(new_buf);
        self.i32store(4)?;

        // Write length
        self.local_get(ret_buf);
        self.i32const(4);
        self.code.push(OpCode::I32Add as u8);
        self.local_get(1);
        self.i32store(4)?;

        self.local_get(ret_buf);
        Ok(Type::I32)
    }

    pub fn get_code(&self) -> &[u8] {
        &self.code
    }

    pub fn get_locals(&self) -> &[VarDecl] {
        &self.locals
    }

    /// Emit expression code. It produces exactly one value onto the stack, so we don't need to return index
    fn emit_expr(&mut self, ast: &Expression) -> Result<Type, String> {
        match ast {
            Expression::LiteralInt(num, ts) => {
                let ret = match (ts.i32, ts.i64) {
                    (true, false) => {
                        self.code.push(OpCode::I32Const as u8);
                        Type::I32
                    }
                    (false, true) => {
                        self.code.push(OpCode::I64Const as u8);
                        Type::I64
                    }
                    _ => {
                        return Err(format!(
                            "Literal int({num}) type could not be determined: {ts}"
                        ))
                    }
                };
                encode_sleb128(&mut self.code, *num).unwrap();
                Ok(ret)
            }
            Expression::LiteralFloat(num, ts) => {
                let ret = match (ts.f32, ts.f64) {
                    (true, false) => {
                        self.code.push(OpCode::F32Const as u8);
                        self.code.write_all(&(*num as f32).to_le_bytes()).unwrap();
                        Type::F32
                    }
                    (false, true) => {
                        self.code.push(OpCode::F64Const as u8);
                        self.code.write_all(&num.to_le_bytes()).unwrap();
                        Type::F64
                    }
                    _ => {
                        return Err(format!(
                            "Literal float({num}) type could not be determined: {ts}"
                        ))
                    }
                };
                Ok(ret)
            }
            Expression::StrLiteral(s) => {
                let str = self.const_table.add_str(s, s);
                self.i32const(str as u32);
                Ok(Type::Str)
            }
            Expression::StructLiteral(stname, fields) => {
                let Some(stdef) = self.structs.get(*stname) else {
                    return Err(format!("Struct {stname} not found"));
                };

                let Some((i_malloc, _)) = self
                    .funcs
                    .iter()
                    .enumerate()
                    .find(|(_, fn_def)| fn_def.name == "malloc")
                else {
                    return Err("malloc not found".to_string());
                };

                let idx_malloc = self.imports.len() + i_malloc;

                self.i32const(stdef.size as u32);

                self.code.push(OpCode::Call as u8);
                encode_leb128(&mut self.code, idx_malloc as u32).unwrap();

                let ptr = self.add_local("", Type::Struct(stname.to_string()));

                for (fname, field) in fields {
                    self.local_get(ptr);
                    let ex = self.emit_expr(field)?;
                    let Some(stfield) = stdef.fields.iter().find(|field| &field.name == fname)
                    else {
                        return Err(format!("Struct field {fname} not found"));
                    };
                    self.code.push(match ex {
                        Type::I32 | Type::Str | Type::Struct(_) => OpCode::I32Store,
                        Type::I64 => OpCode::I64Store,
                        Type::F32 => OpCode::F32Store,
                        Type::F64 => OpCode::F64Store,
                        Type::Void => {
                            return Err(format!("Struct field {} has a void type", stfield.name))
                        }
                    } as u8);
                    encode_leb128(&mut self.code, 0).unwrap();
                    encode_leb128(&mut self.code, stfield.offset as u32).unwrap();
                    println!("field {fname}: offset {offset}", offset = stfield.offset);
                }

                self.local_get(ptr);

                Ok(Type::Struct(stname.to_string()))
            }
            Expression::Variable(name) => {
                let (ret, local) = self
                    .locals
                    .iter()
                    .enumerate()
                    .find(|(_, local)| &local.name == name)
                    .ok_or_else(|| format!("Variable {name} not found"))?;
                let ty = local.ty.determine().unwrap();
                self.local_get(ret);
                Ok(ty)
            }
            Expression::FnInvoke(name, args) => {
                let idx;
                let fn_ty;
                if let Some((i, import)) = self
                    .imports
                    .iter()
                    .enumerate()
                    .find(|(_, f)| f.name == *name)
                {
                    idx = i;
                    fn_ty = import.ty;
                } else if let Some((i, func)) =
                    self.funcs.iter().enumerate().find(|(_, f)| f.name == *name)
                {
                    idx = i + self.imports.len();
                    fn_ty = func.ty;
                } else {
                    return Err(format!("Calling undefined function {}", name));
                };
                for arg in args {
                    if Type::Void == self.emit_expr(arg)? {
                        return Err("Function argument requires a value".to_string());
                    }
                }
                self.code.push(OpCode::Call as u8);
                encode_leb128(&mut self.code, idx as u32).unwrap();
                let fn_ty = &self.types[fn_ty];
                Ok(fn_ty.results.get(0).cloned().unwrap_or(Type::Void))
            }
            Expression::Cast(ex, ty) => {
                let ex_ty = self.emit_expr(ex)?;
                self.coerce_type(ty, &ex_ty)?;
                Ok(ty.clone())
            }
            Expression::FieldAccess(ex, fname) => {
                let ex_ty = self.emit_expr(ex)?;
                let Type::Struct(stname) = ex_ty else {
                    return Err("Field access operator applied to non-struct".to_string());
                };
                let Some(stdef) = self.structs.get(&stname) else {
                    return Err(format!("Struct {stname} not found"));
                };
                let Some(stfield) = stdef.fields.iter().find(|field| &field.name == fname) else {
                    return Err(format!("Struct field {fname} not found"));
                };
                self.field_load(stfield)?;
                Ok(stfield.ty.clone())
            }
            Expression::Neg(ex) => {
                match self.emit_expr(ex)? {
                    // Since i32 and i64 don't have negation opcode, we need to add a local variable and
                    // subtract it from 0.
                    Type::I32 => {
                        let v = self.add_local("", Type::I32);
                        self.code.push(OpCode::I32Const as u8);
                        encode_leb128(&mut self.code, 0).unwrap();
                        self.code.push(OpCode::LocalGet as u8);
                        encode_leb128(&mut self.code, v as u32).unwrap();
                        self.code.push(OpCode::I32Sub as u8);
                        Ok(Type::I32)
                    }
                    Type::I64 => {
                        let v = self.add_local("", Type::I32);
                        self.code.push(OpCode::I32Const as u8);
                        encode_leb128(&mut self.code, 0).unwrap();
                        self.code.push(OpCode::LocalGet as u8);
                        encode_leb128(&mut self.code, v as u32).unwrap();
                        self.code.push(OpCode::I32Sub as u8);
                        Ok(Type::I64)
                    }
                    Type::F32 => {
                        self.code.push(OpCode::F32Neg as u8);
                        Ok(Type::F32)
                    }
                    Type::F64 => {
                        self.code.push(OpCode::F64Neg as u8);
                        Ok(Type::F64)
                    }
                    Type::Void => Ok(Type::Void),
                    Type::Str => return Err("Unary minus is not defined for str".to_string()),
                    Type::Struct(_) => {
                        return Err("Unary minus is not defined for struct".to_string())
                    }
                }
            }
            Expression::Add(lhs, rhs) => self.emit_bin_op(
                lhs,
                rhs,
                "add",
                TypeMap {
                    i32: OpCode::I32Add,
                    i64: OpCode::I64Add,
                    f32: OpCode::F32Add,
                    f64: OpCode::F64Add,
                    st: Some(Box::new(|this| {
                        this.call_func("strcat")?;
                        Ok(())
                    })),
                },
            ),
            Expression::Sub(lhs, rhs) => self.emit_bin_op(
                lhs,
                rhs,
                "sub",
                TypeMap {
                    i32: OpCode::I32Sub,
                    i64: OpCode::I64Sub,
                    f32: OpCode::F32Sub,
                    f64: OpCode::F64Sub,
                    st: None,
                },
            ),
            Expression::Mul(lhs, rhs) => self.emit_bin_op(
                lhs,
                rhs,
                "mul",
                TypeMap {
                    i32: OpCode::I32Mul,
                    i64: OpCode::I64Mul,
                    f32: OpCode::F32Mul,
                    f64: OpCode::F64Mul,
                    st: None,
                },
            ),
            Expression::Div(lhs, rhs) => self.emit_bin_op(
                lhs,
                rhs,
                "div",
                TypeMap {
                    i32: OpCode::I32DivS,
                    i64: OpCode::I64DivS,
                    f32: OpCode::F32Div,
                    f64: OpCode::F64Div,
                    st: None,
                },
            ),
            Expression::Lt(lhs, rhs) => self.emit_cmp_op(lhs, rhs, "lt", LT_TYPE_MAP),
            Expression::Gt(lhs, rhs) => self.emit_cmp_op(
                lhs,
                rhs,
                "gt",
                TypeMap {
                    i32: OpCode::I32GtS,
                    i64: OpCode::I64GtS,
                    f32: OpCode::F32Gt,
                    f64: OpCode::F64Gt,
                    st: None,
                },
            ),
            Expression::Conditional(cond, t_branch, f_branch) => {
                self.emit_expr(cond)?;
                self.code.push(OpCode::If as u8);
                let ty_fixup = self.code.len();
                self.code.push(Type::Void.code());

                let t_result = self.emit_stmts(t_branch, &Type::Void)?;

                self.code[ty_fixup] = t_result.code();

                if let Some(f_branch) = f_branch {
                    self.code.push(OpCode::Else as u8);
                    let f_result = self.emit_stmts(f_branch, &Type::Void)?;
                    if f_result != t_result {
                        return Err(format!("True branch type {t_result} and false branch type {f_result} does not match"));
                    }
                } else if t_result != Type::Void {
                    return Err("True branch with yielded value requires false branch".to_string());
                }

                self.code.push(OpCode::End as u8);

                Ok(t_result)
            }
        }
    }

    /// Emit lvalue from expression. It produces exactly one value onto the stack, so we don't need to return index
    fn emit_lvalue(&mut self, ast: &Expression) -> Result<(LValue, Type), String> {
        match ast {
            Expression::LiteralInt(_, _) => Err("Literal int cannot be a lvalue".to_string()),
            Expression::LiteralFloat(_, _) => Err("Literal float cannot be a lvalue".to_string()),
            Expression::StrLiteral(_) => Err("Literal string cannot be a lvalue".to_string()),
            Expression::StructLiteral(_, _) => Err("Literal struct cannot be a lvalue".to_string()),
            Expression::Variable(name) => {
                let (ret, local) = self
                    .locals
                    .iter()
                    .enumerate()
                    .find(|(_, local)| &local.name == name)
                    .ok_or_else(|| format!("Variable {name} not found"))?;
                Ok((
                    LValue::Local(ret),
                    local
                        .ty
                        .determine()
                        .ok_or_else(|| "Type could not be determined".to_string())?,
                ))
            }
            Expression::FnInvoke(_, _) => {
                Err("Function return value cannot be a lvalue".to_string())
            }
            Expression::Cast(_, _) => Err("Cast expression cannot be a lvalue".to_string()),
            Expression::FieldAccess(ex, fname) => {
                let (st, ty) = self.emit_lvalue(ex)?;

                let Type::Struct(stname) = &ty else {
                    return Err("Field access operator applied to a non-struct".to_string());
                };

                let Some(stdef) = self.structs.get(stname) else {
                    return Err(format!("Struct {stname} was not found"));
                };

                let Some(stfield) = stdef.fields.iter().find(|f| &f.name == fname) else {
                    return Err(format!("Struct field {fname} was not found"));
                };

                match st {
                    LValue::Local(local) => {
                        self.local_get(local);
                        Ok((LValue::Memory, stfield.ty.clone()))
                    }
                    LValue::Memory => {
                        self.i32load(0)?;
                        Ok((LValue::Memory, stfield.ty.clone()))
                    }
                }
            }
            Expression::Neg(_)
            | Expression::Add(_, _)
            | Expression::Sub(_, _)
            | Expression::Mul(_, _)
            | Expression::Div(_, _)
            | Expression::Lt(_, _)
            | Expression::Gt(_, _)
            | Expression::Conditional(_, _, _) => {
                Err("Arithmetic expression cannot be a lvalue".to_string())
            }
        }
    }

    fn local_get(&mut self, idx: usize) {
        self.code.push(OpCode::LocalGet as u8);
        encode_leb128(&mut self.code, idx as u32).unwrap();
    }

    fn emit_bin_op(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        name: &str,
        ty_map: TypeMap,
    ) -> Result<Type, String> {
        let lhs = self.emit_expr(lhs)?;
        let rhs = self.emit_expr(rhs)?;
        let (op, ty) = match (&lhs, &rhs) {
            (Type::I32, Type::I32) => (ty_map.i32, lhs),
            (Type::I64, Type::I64) => (ty_map.i64, lhs),
            (Type::F32, Type::F32) => (ty_map.f32, lhs),
            (Type::F64, Type::F64) => (ty_map.f64, lhs),
            (Type::I32, Type::F64) => {
                let lhs_local = self.add_local("", Type::F64);
                self.code.push(OpCode::F64ConvertI32S as u8);
                self.local_get(lhs_local);
                (ty_map.f64, Type::F64)
            }
            (Type::F64, Type::I32) => {
                self.code.push(OpCode::F64ConvertI32S as u8);
                (ty_map.f64, Type::F64)
            }
            (Type::Str, Type::Str) => {
                if let Some(f) = ty_map.st.as_ref() {
                    f(self)?;
                    return Ok(Type::Str);
                } else {
                    return Err(format!("Type mismatch for {name:?}: {lhs} and {rhs}"));
                }
            }
            _ => return Err(format!("Type mismatch for {name:?}: {lhs} and {rhs}")),
        };
        self.code.push(op as u8);
        Ok(ty)
    }

    fn emit_cmp_op(
        &mut self,
        lhs: &Expression,
        rhs: &Expression,
        name: &str,
        ty_map: TypeMap,
    ) -> Result<Type, String> {
        let lhs = self.emit_expr(lhs)?;
        let rhs = self.emit_expr(rhs)?;
        let op = match (&lhs, &rhs) {
            (Type::I32, Type::I32) => ty_map.i32,
            (Type::I64, Type::I64) => ty_map.i64,
            (Type::F32, Type::F32) => ty_map.f32,
            (Type::F64, Type::F64) => ty_map.f64,
            (Type::I32, Type::F64) => {
                let lhs_local = self.add_local("", Type::F64);
                self.code.push(OpCode::F64ConvertI32S as u8);
                self.local_get(lhs_local);
                ty_map.f64
            }
            (Type::F64, Type::I32) => {
                self.code.push(OpCode::F64ConvertI32S as u8);
                ty_map.f64
            }
            _ => return Err(format!("Type mismatch for {name:?}: {lhs} and {rhs}")),
        };
        self.code.push(op as u8);
        Ok(Type::I32)
    }

    /// Returns if a value is pushed to the stack
    fn emit_stmt(&mut self, stmt: &Statement, ty: &Type) -> Result<Type, String> {
        match stmt {
            Statement::Expr(ex) => self.emit_expr(ex),
            Statement::VarDecl(name, ty, ex) => {
                let ex_ty = self.emit_expr(ex)?;
                let ty = ty.determine().unwrap();
                self.coerce_type(&ty, &ex_ty).map_err(|_| {
                    format!(
                        "Variable declared type {ty} and initializer type {ex_ty} are different"
                    )
                })?;
                self.add_local(*name, ty);
                Ok(Type::Void)
            }
            Statement::VarAssign(lhs, ex) => {
                let (lvalue, ty) = self.emit_lvalue(lhs)?;
                self.emit_expr(ex)?;
                match lvalue {
                    LValue::Local(local) => {
                        self.code.push(OpCode::LocalSet as u8);
                        encode_leb128(&mut self.code, local as u32).unwrap();
                    }
                    LValue::Memory => match ty {
                        Type::I32 | Type::Str | Type::Struct(_) => self.i32store(0)?,
                        Type::I64 => self.i64store(0)?,
                        Type::F32 => self.f32store(0)?,
                        Type::F64 => self.f64store(0)?,
                        Type::Void => return Err("Cannot store to a void".to_string()),
                    },
                }
                Ok(Type::Void)
            }
            Statement::FnDecl(_) => {
                // We do not compile subfunction at this point.
                Ok(Type::Void)
            }
            Statement::For(For {
                name,
                start,
                end,
                stmts,
            }) => {
                let start_ty = self.emit_expr(start)?;
                let idx = self.add_local(*name, start_ty.clone());

                let end_ty = self.emit_expr(end)?;
                let end = self.add_local("", end_ty.clone());

                if start_ty != end_ty {
                    return Err(format!(
                        "Start and end types do not match: {start_ty} and {end_ty}"
                    ));
                }

                self.emit_for_loop(idx, end, start_ty, |this| {
                    this.emit_stmts(stmts, &Type::Void).map(|_| ())
                })?;

                Ok(Type::Void)
            }
            Statement::Brace(stmts) => {
                let res_ty = self.emit_stmts(stmts, ty)?;
                if res_ty != Type::Void {
                    self.coerce_type(&ty, &res_ty)?;
                }
                Ok(Type::Void)
            }
            Statement::Return(ex) => {
                if let Some(ex) = ex {
                    let ex_ty = self.emit_expr(ex)?;
                    let ret_ty = self.ret_ty.clone();
                    self.coerce_type(&ret_ty, &ex_ty)?;
                }
                self.code.push(OpCode::Return as u8);
                Ok(Type::Void)
            }
            Statement::Struct(_) => Ok(Type::Void),
        }
    }

    fn emit_stmts(&mut self, stmts: &[Statement], ty: &Type) -> Result<Type, String> {
        let mut last_ty = Type::Void;
        if 1 <= stmts.len() {
            for stmt in &stmts[..stmts.len() - 1] {
                for _ in 0..last_ty.word_count() {
                    self.code.push(OpCode::Drop as u8);
                }
                last_ty = self.emit_stmt(stmt, &Type::Void)?;
            }
        }
        if let Some(last_stmt) = stmts.last() {
            for _ in 0..last_ty.word_count() {
                self.code.push(OpCode::Drop as u8);
            }
            last_ty = self.emit_stmt(last_stmt, ty)?;
        }

        Ok(last_ty)
    }

    /// Add instructions to pop a value from the stack and puts it as a local variable.
    /// Returns the local index of added local variable.
    fn add_local(&mut self, name: impl Into<String>, ty: Type) -> usize {
        let ret = self.locals.len();
        self.code.push(OpCode::LocalSet as u8);
        encode_leb128(&mut self.code, ret as u32).unwrap();
        self.locals.push(VarDecl {
            name: name.into(),
            ty: ty.into(),
        });
        ret
    }

    fn coerce_type(&mut self, ty: &Type, ex: &Type) -> Result<(), String> {
        match (ty, ex) {
            (Type::I32, Type::I32)
            | (Type::I64, Type::I64)
            | (Type::F32, Type::F32)
            | (Type::F64, Type::F64)
            | (Type::Void, Type::Void)
            | (Type::Str, Type::Str) => {}
            (Type::I32, Type::I64) => self.code.push(OpCode::I32WrapI64 as u8),
            (Type::I64, Type::I32) => self.code.push(OpCode::I64ExtendI32S as u8),
            (Type::I32, Type::F64) => self.code.push(OpCode::I32TruncF64S as u8),
            (Type::F64, Type::I32) => self.code.push(OpCode::F64ConvertI32S as u8),
            (Type::Struct(ty), Type::Struct(ex)) => {
                if ty != ex {
                    return Err(format!("Coercing type {ty:?} from type {ex:?} failed"));
                }
            }
            _ => return Err(format!("Coercing type {ty:?} from type {ex:?} failed")),
        }
        Ok(())
    }

    /// Emit a for loop running while incrementing `idx` until `end`.
    fn emit_for_loop(
        &mut self,
        idx: usize,
        end: usize,
        iter_ty: Type,
        gen_code: impl FnOnce(&mut Self) -> Result<(), String>,
    ) -> Result<(), String> {
        // Start block
        self.code.push(OpCode::Block as u8);
        self.code.push(Type::Void.code());

        // Start loop
        self.code.push(OpCode::Loop as u8);
        self.code.push(Type::Void.code());

        // End condition
        self.local_get(idx);
        self.local_get(end);
        self.code.push(match iter_ty {
            Type::I32 => OpCode::I32GeS,
            Type::I64 => OpCode::I64GeS,
            Type::F32 => OpCode::F32Ge,
            Type::F64 => OpCode::F64Ge,
            _ => return Err("For loop iteration variable has void type".to_string()),
        } as u8);
        self.code.push(OpCode::BrIf as u8);
        encode_leb128(&mut self.code, 1).unwrap();

        gen_code(self)?;

        // Incr idx
        self.local_get(idx);
        match iter_ty {
            Type::I32 => {
                self.code.push(OpCode::I32Const as u8);
                encode_leb128(&mut self.code, 1).unwrap();
                self.code.push(OpCode::I32Add as u8);
            }
            Type::I64 => {
                self.code.push(OpCode::I64Const as u8);
                encode_leb128(&mut self.code, 1).unwrap();
                self.code.push(OpCode::I64Add as u8);
            }
            Type::F32 => {
                self.code.push(OpCode::F32Const as u8);
                self.code.extend_from_slice(&1f32.to_le_bytes());
                self.code.push(OpCode::F32Add as u8);
            }
            Type::F64 => {
                self.code.push(OpCode::F64Const as u8);
                self.code.extend_from_slice(&1f64.to_le_bytes());
                self.code.push(OpCode::F64Add as u8);
            }
            _ => return Err("For loop iteration variable has void type".to_string()),
        }
        self.code.push(OpCode::LocalSet as u8);
        encode_leb128(&mut self.code, idx as u32).unwrap();

        self.code.push(OpCode::Br as u8);
        encode_leb128(&mut self.code, 0).unwrap();

        // end loop
        self.code.push(OpCode::End as u8);

        // end block
        self.code.push(OpCode::End as u8);
        Ok(())
    }
}

pub(crate) fn encode_leb128(f: &mut impl Write, value: u32) -> std::io::Result<()> {
    let mut value = value as u32;
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            byte |= 0x80;
        }
        f.write_all(&[byte])?;
        if value == 0 {
            return Ok(());
        }
    }
}

#[test]
fn test_leb128() {
    let mut v = vec![];
    encode_leb128(&mut v, 256).unwrap();
    assert_eq!(v, vec![0x80, 0x02]);
}

pub(crate) fn encode_sleb128(f: &mut impl Write, value: impl Into<i64>) -> std::io::Result<()> {
    let mut value = value.into();
    let mut more = true;
    while more {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if (value == 0 && byte & 0x40 == 0) || (value == -1 && byte & 0x40 != 0) {
            more = false;
        } else {
            byte |= 0x80;
        }
        f.write_all(&[byte])?;
    }
    return Ok(());
}

#[test]
fn test_sleb128() {
    let mut v = vec![];
    encode_sleb128(&mut v, -123456).unwrap();
    assert_eq!(v, vec![0xc0, 0xbb, 0x78]);
    assert_eq!(
        decode_sleb128(&mut std::io::Cursor::new(&v)).unwrap(),
        -123456
    );
}

pub(crate) fn decode_leb128(f: &mut impl Read) -> std::io::Result<i32> {
    let mut value = 0u32;
    let mut shift = 0;
    loop {
        let mut byte = [0u8];
        f.read_exact(&mut byte)?;
        value |= ((byte[0] & 0x7f) as u32) << shift;
        if byte[0] & 0x80 == 0 {
            return Ok(value as i32);
        }
        shift += 7;
    }
}

pub(crate) fn decode_sleb128(f: &mut impl Read) -> std::io::Result<i32> {
    let mut value = 0u32;
    let mut shift = 0;
    let mut byte = [0u8];
    loop {
        f.read_exact(&mut byte)?;
        value |= ((byte[0] & 0x7f) as u32) << shift;
        shift += 7;
        if byte[0] & 0x80 == 0 {
            break;
        }
    }
    if shift < std::mem::size_of::<i32>() * 8 && byte[0] & 0x40 != 0 {
        value |= !0 << shift;
    }
    return Ok(value as i32);
}

pub fn disasm(code: &[u8], f: &mut impl Write) -> std::io::Result<()> {
    use OpCode::*;
    let mut cur = std::io::Cursor::new(code);
    let mut block_level = 1;
    loop {
        let mut op_code_buf = [0u8];
        cur.read_exact(&mut op_code_buf)?;
        let op_code = op_code_buf[0];
        let indent = "  ".repeat(block_level);
        let code = OpCode::from(op_code);
        match code {
            Block => {
                let mut ty = [0u8];
                cur.read_exact(&mut ty)?;
                writeln!(f, "{indent}block (result {})", Type::from(ty[0]))?;
                block_level += 1;
            }
            Loop => {
                let mut ty = [0u8];
                cur.read_exact(&mut ty)?;
                writeln!(f, "{indent}loop (result {})", Type::from(ty[0]))?;
                block_level += 1;
            }
            If => {
                let mut ty = [0u8];
                cur.read_exact(&mut ty)?;
                writeln!(f, "{indent}if (result {})", Type::from(ty[0]))?;
                block_level += 1;
            }
            Else => {
                writeln!(f, "{indent}else")?;
            }
            Br => {
                let mut label = [0u8];
                cur.read_exact(&mut label)?;
                writeln!(f, "{indent}br {}", label[0])?;
            }
            BrIf => {
                let mut label = [0u8];
                cur.read_exact(&mut label)?;
                writeln!(f, "{indent}br_if {}", label[0])?;
            }
            Return => {
                writeln!(f, "{indent}return")?;
            }
            Call => {
                let arg = decode_leb128(&mut cur)?;
                writeln!(f, "{indent}call {arg}")?;
            }
            Drop => {
                writeln!(f, "{indent}drop")?;
            }
            LocalGet => {
                let arg = decode_leb128(&mut cur)?;
                writeln!(f, "{indent}local.get {arg}")?;
            }
            LocalSet => {
                let arg = decode_leb128(&mut cur)?;
                writeln!(f, "{indent}local.set {arg}")?;
            }
            I32Store | I64Store | F32Store | F64Store | I32Store8 | I64Store8 | I32Load
            | I32Load8S | I64Load | F32Load | F64Load => {
                let mem = decode_leb128(&mut cur)?;
                let align = decode_leb128(&mut cur)?;
                writeln!(f, "{indent}{} {mem} {align}", code.to_name())?;
            }
            I32Const => {
                let arg = decode_sleb128(&mut cur)?;
                writeln!(f, "{indent}i32.const {arg}")?;
            }
            I64Const => {
                let arg = decode_sleb128(&mut cur)?;
                writeln!(f, "{indent}i64.const {arg}")?;
            }
            F32Const => {
                let mut buf = [0u8; std::mem::size_of::<f32>()];
                cur.read_exact(&mut buf)?;
                let arg = f32::from_le_bytes(buf);
                writeln!(f, "{indent}f32.const {arg}")?;
            }
            F64Const => {
                let mut buf = [0u8; std::mem::size_of::<f64>()];
                cur.read_exact(&mut buf)?;
                let arg = f64::from_le_bytes(buf);
                writeln!(f, "{indent}f64.const {arg}")?;
            }
            I32LtS | I32LtU | I32GtS | I32GtU | I32LeS | I32LeU | I32GeS | I32GeU | I32Add
            | I32Sub | I32Mul | I32DivS | I32And | I64LtS | I64LtU | I64GtS | I64GtU | I64LeS
            | I64LeU | I64GeS | I64GeU | I64Add | I64Sub | I64Mul | I64DivS | F32Neg | F32Lt
            | F32Gt | F32Le | F32Ge | F32Add | F32Sub | F32Mul | F32Div | F64Neg | F64Lt
            | F64Gt | F64Le | F64Ge | F64Add | F64Sub | F64Mul | F64Div | I32WrapI64
            | I32TruncF32S | I32TruncF64S | I64ExtendI32S | I64TruncF32S | I64TruncF64S
            | F32ConvertI32S | F32ConvertI64S | F32DemoteF64 | F64ConvertI32S | F64ConvertI64S
            | F64PromoteF32 => {
                writeln!(f, "{indent}{}", code.to_name())?;
            }
            End => {
                writeln!(f, "{indent}end")?;
                block_level -= 1;
                if block_level == 0 {
                    return Ok(());
                };
            }
        }
    }
}

pub fn disasm_func(
    func: &FuncDef,
    func_ty: &FuncType,
    out: &mut impl Write,
) -> std::io::Result<()> {
    let params = &func.locals[..func_ty.params.len()];
    let params = params.iter().fold("".to_string(), |mut acc, cur| {
        if !acc.is_empty() {
            acc += ", ";
        }
        acc += &format!("{}: {}", cur.name, cur.ty);
        acc
    });

    writeln!(
        out,
        "Disasm {}{}({}) -> {}: ",
        if func.public { "pub " } else { "" },
        func.name,
        params,
        func_ty.results.first().unwrap_or(&Type::Void)
    )?;
    let locals = &func.locals[func_ty.params.len()..];
    let locals = locals
        .iter()
        .enumerate()
        .fold("".to_string(), |mut acc, (i, cur)| {
            if !acc.is_empty() {
                acc += ", ";
            }
            acc += &format!("[{}] {}: {}", i + func_ty.params.len(), cur.name, cur.ty);
            acc
        });
    writeln!(out, "  locals: {locals}")?;

    disasm(&func.code, out)
}
