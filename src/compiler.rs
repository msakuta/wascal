mod alloca;
mod count_stack;
mod disasm;
mod emit;
mod malloc;
mod reverse;
mod set;
mod sqrt;
mod strcat;

use std::{collections::HashMap, io::Write};

use self::emit::{to_load, to_store};

use crate::{
    const_table::ConstTable,
    leb128::{encode_leb128, encode_sleb128},
    model::{FuncDef, FuncImport, FuncType, StructDef, Type},
    parser::{Expression, For, Statement, VarDecl},
};

pub use self::disasm::disasm_func;

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
    MemorySize = 0x3f,
    MemoryGrow = 0x40,
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
    I32DivU = 0x6e,
    I32And = 0x71,
    I32Or = 0x72,
    I64Add = 0x7c,
    I64Sub = 0x7d,
    I64Mul = 0x7e,
    I64DivS = 0x7f,
    F32Neg = 0x8c,
    F32Sqrt = 0x91,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    F64Neg = 0x9a,
    F64Sqrt = 0x9f,
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
    MemorySize: "memory.size",
    MemoryGrow: "memory.grow",
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
    I32DivU: "i32.div_u",
    I32And: "i32.and",
    I32Or: "i32.or",
    I64Add: "i64.add",
    I64Sub: "i64.sub",
    I64Mul: "i64.mul",
    I64DivS: "i64.div_s",
    F32Neg: "f32.neg",
    F32Sqrt: "f32.sqrt",
    F32Add: "f32.add", F32Sub: "f32.sub", F32Mul: "f32.mul", F32Div: "f32.div",
    F64Neg: "f64.neg",
    F64Sqrt: "f64.sqrt",
    F64Add: "f64.add", F64Sub: "f64.sub", F64Mul: "f64.mul", F64Div: "f64.div",
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
    Memory(usize),
}

/// A environment for compiling a function. Note that a program is made of multiple functions,
/// so you need multiple instances of this object.
pub struct Compiler<'a> {
    code: Vec<u8>,
    args: usize,
    locals: Vec<VarDecl>,
    ret_ty: Type,
    types: &'a mut Vec<FuncType>,
    imports: &'a [FuncImport],
    const_table: &'a mut ConstTable,
    /// References to other functions to call and type check.
    funcs: &'a mut Vec<FuncDef>,
    structs: &'a HashMap<String, StructDef>,
    local_ty: HashMap<String, Type>,
    /// A local index indicating the base pointer, the address of the bottom of the linear memory stack.
    base_ptr: usize,
    /// An accumulator to keep track of how many bytes are used in the stack in emit_*
    base_offset: usize,
    /// The size of the linear memory stack, in bytes, not to be confused with the locals
    stack_size: usize,
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
    ) -> Result<Self, String> {
        let local_ty = args
            .iter()
            .map(|arg| Some((arg.name.clone(), arg.ty.clone().determine()?)))
            .collect::<Option<_>>()
            .ok_or_else(|| format!("Argument type could not be determined"))?;
        let mut locals = args;
        let args = locals.len();
        if matches!(ret_ty, Type::Struct(_)) {
            locals.push(VarDecl {
                name: "return".to_string(),
                ty: ret_ty.clone().into(),
            });
        }
        Ok(Self {
            code: vec![],
            args,
            locals,
            ret_ty,
            types,
            imports,
            const_table,
            funcs,
            structs,
            local_ty,
            base_ptr: 0,
            base_offset: 0,
            stack_size: 0,
        })
    }

    pub fn compile(&mut self, ast: &[Statement], ty: Type) -> Result<Type, String> {
        self.stack_size = self.count_stack_stmts(ast)?;

        println!("stack size is {}", self.stack_size);

        if 0 < self.stack_size {
            // Remember base pointer, the address of the top of the stack before calling this function.
            // It forms a stack in linear memory. Not to be confused with operand stack in Wasm runtime.
            self.i32const(4);
            self.i32load(0)?;
            self.base_ptr = self.add_local("", Type::I32);
            self.i32const(self.stack_size as u32);
            self.call_func("alloca")?;
            self.code.push(OpCode::Drop as u8);
        }

        let last = self.emit_stmts(ast, &ty)?;

        self.return_code()?;

        self.code.push(OpCode::End as u8);
        Ok(last)
    }

    fn return_code(&mut self) -> Result<(), String> {
        // The calling convention for non-primitive type is that
        //   <arg0> <arg1> ... <arg n-1> <return_addr>
        // where return_addr points to the calling function's stack address that
        // holds the result.
        // TODO: RVO
        if let Some(stname) = self.ret_ty.struct_name() {
            let st = self
                .structs
                .get(stname)
                .ok_or_else(|| format!("Struct {stname} not found"))?;
            let local_buf = self.add_local("", Type::I32);
            let ret_buf = self.args;
            for field in &st.fields {
                if let Some((store_op, load_op)) = to_store(&field.ty).zip(to_load(&field.ty)) {
                    self.local_get(ret_buf);
                    self.local_get(local_buf);
                    self.load(load_op, field.offset as u32)?;
                    self.store(store_op, field.offset as u32)?;
                }
            }
        }

        if 0 < self.stack_size {
            // Restore base pointer
            self.i32const(4);
            self.local_get(self.base_ptr);
            self.i32store(0)?;
        }

        Ok(())
    }

    /// Find a function defined in this Wasm module, i.e. not an imported function.
    fn find_local_func(&self, name: &str) -> Result<(usize, Type), String> {
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
        let (func_id, ret_ty) = self.find_local_func(name)?;
        self.code.push(OpCode::Call as u8);
        encode_leb128(&mut self.code, func_id as u32).map_err(|e| e.to_string())?;
        Ok(ret_ty)
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

                // We need to collect to a vec because self.base_ptr can change
                let fields = fields
                    .into_iter()
                    .map(|(fname, field)| {
                        self.local_get(self.base_ptr);
                        let ex = self.emit_expr(field)?;
                        let stfield = stdef
                            .fields
                            .iter()
                            .find(|field| &field.name == fname)
                            .ok_or_else(|| format!("Struct field {fname} not found"))?;
                        let op = match ex {
                            Type::I32 | Type::Str | Type::Struct(_) => OpCode::I32Store,
                            Type::I64 => OpCode::I64Store,
                            Type::F32 => OpCode::F32Store,
                            Type::F64 => OpCode::F64Store,
                            Type::Void => {
                                return Err(format!(
                                    "Struct field {} has a void type",
                                    stfield.name
                                ))
                            }
                        };
                        Ok((op, stfield))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                // reverse because values are pushed to stack
                for (op, stfield) in fields.into_iter().rev() {
                    self.store(op, (self.base_offset + stfield.offset) as u32)?;
                }

                self.local_get(self.base_ptr);
                self.i32const(self.base_offset as u32);
                self.code.push(OpCode::I32Add as u8);

                self.base_offset += stdef.size;

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
                for arg in args {
                    if Type::Void == self.emit_expr(arg)? {
                        return Err("Function argument requires a value".to_string());
                    }
                }

                let (func_idx, ret_ty, _) = self
                    .find_func(name)
                    .ok_or_else(|| format!("Calling undefined function {name}"))?;

                self.emit_call_func(func_idx, ret_ty)
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
            Expression::And(lhs, rhs) => {
                // TODO: short-circuit
                self.emit_expr(lhs)?;
                self.emit_expr(rhs)?;
                self.code.push(OpCode::I32And as u8);
                Ok(Type::I32)
            }
            Expression::Or(lhs, rhs) => {
                // TODO: short-circuit
                self.emit_expr(lhs)?;
                self.emit_expr(rhs)?;
                self.code.push(OpCode::I32Or as u8);
                Ok(Type::I32)
            }
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
                    }
                    LValue::Memory(offset) => {
                        self.i32load(offset as u32)?;
                    }
                }
                Ok((LValue::Memory(stfield.offset), stfield.ty.clone()))
            }
            Expression::Neg(_)
            | Expression::Add(_, _)
            | Expression::Sub(_, _)
            | Expression::Mul(_, _)
            | Expression::Div(_, _)
            | Expression::Lt(_, _)
            | Expression::Gt(_, _)
            | Expression::And(_, _)
            | Expression::Or(_, _)
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
            (Type::Struct(_), Type::Struct(_)) => {
                let dunder = format!("__{name}__");
                let (func_idx, ret_ty, func) = self
                    .find_func(&dunder)
                    .ok_or_else(|| format!("Calling undefined function {}", dunder))?;
                if let Some(func) = func {
                    if func.locals.get(0).map(|lo| &lo.ty) != Some(&lhs.clone().into()) {
                        return Err(format!("Type mismatch for {name:?}: {lhs} and {rhs}"));
                    }
                    if func.locals.get(1).map(|lo| &lo.ty) != Some(&rhs.clone().into()) {
                        return Err(format!("Type mismatch for {name:?}: {lhs} and {rhs}"));
                    }
                }

                let ret_ty = self.emit_call_func(func_idx, ret_ty)?;

                return Ok(ret_ty);
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

    /// Find a function by name, either user defined or imported, and return its index, return type
    /// and optionally a reference to FuncDef, if it is user defined function.
    fn find_func(&self, name: &str) -> Option<(usize, Type, Option<&FuncDef>)> {
        let idx;
        let ret_ty;
        let func_def;
        if let Some((i, import)) = self
            .imports
            .iter()
            .enumerate()
            .find(|(_, f)| f.name == *name)
        {
            idx = i;
            ret_ty = self.types[import.ty]
                .results
                .get(0)
                .cloned()
                .unwrap_or(Type::Void);
            func_def = None;
        } else if let Some((i, func)) = self.funcs.iter().enumerate().find(|(_, f)| f.name == *name)
        {
            idx = i + self.imports.len();
            ret_ty = func.ret_ty.clone();
            func_def = Some(func);
        } else {
            return None;
        };
        Some((idx, ret_ty, func_def))
    }

    fn emit_call_func(&mut self, func_idx: usize, ret_ty: Type) -> Result<Type, String> {
        // If the returned value is a struct, allocate memory on the calling function
        let ret_local = if let Some(stname) = ret_ty.struct_name() {
            let st = self
                .structs
                .get(stname)
                .ok_or_else(|| format!("Struct {stname} is not found"))?;
            println!("Calling function returning a struct {stname}");
            self.local_get(self.base_ptr);
            self.i32const(self.base_offset as u32);
            self.code.push(OpCode::I32Add as u8);
            let local = self.add_local("", ret_ty.clone());
            self.local_get(local);

            self.base_offset += st.size;

            Some(local)
        } else {
            None
        };

        self.code.push(OpCode::Call as u8);
        encode_leb128(&mut self.code, func_idx as u32).unwrap();

        if let Some(ret_local) = ret_local {
            self.local_get(ret_local);
        }

        Ok(ret_ty)
    }

    /// Returns if a value is pushed to the stack
    fn emit_stmt(&mut self, stmt: &Statement, ty: &Type) -> Result<Type, String> {
        match stmt {
            Statement::Expr(ex, semicolon) => {
                let ex_ty = self.emit_expr(ex)?;
                if *semicolon {
                    self.coerce_type(&Type::Void, &ex_ty)?;
                    Ok(Type::Void)
                } else {
                    Ok(ex_ty)
                }
            }
            Statement::VarDecl(name, ty, ex) => {
                let ex_ty = self.emit_expr(ex)?;
                let ty = ty.determine().ok_or_else(|| {
                    format!("Variable {name} type should be determinable but got {}", ty)
                })?;
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
                    LValue::Memory(offset) => match ty {
                        Type::I32 | Type::Str | Type::Struct(_) => self.i32store(offset as u32)?,
                        Type::I64 => self.i64store(offset as u32)?,
                        Type::F32 => self.f32store(offset as u32)?,
                        Type::F64 => self.f64store(offset as u32)?,
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
            (Type::Void, Type::I32 | Type::I64 | Type::F32 | Type::F64) => {
                self.code.push(OpCode::Drop as u8);
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
