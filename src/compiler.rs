use std::io::{Read, Write};

use crate::{
    model::{FuncDef, FuncImport, FuncType, Type},
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
    I32Const = 0x41,
    F64Const = 0x44,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I32GtS = 0x4a,
    I32GtU = 0x4b,
    I64LtS = 0x53,
    I64LtU = 0x54,
    I64GtS = 0x55,
    I64GtU = 0x56,
    F32Lt = 0x5d,
    F32Gt = 0x5e,
    F64Lt = 0x63,
    F64Gt = 0x64,
    I32Add = 0x6a,
    I32Sub = 0x6b,
    I32Mul = 0x6c,
    I32DivS = 0x6d,
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
    LocalGet: "local.get", LocalSet: "local.set", I32Const: "i32.const", F64Const: "f64.const",
    I32LtS: "i32.lt_s",
    I32LtU: "i32.lt_u",
    I32GtS: "i32.gt_s",
    I32GtU: "i32.gt_u",
    I64LtS: "i64.lt_s",
    I64LtU: "i64.lt_u",
    I64GtS: "i64.gt_s",
    I64GtU: "i64.gt_u",
    F32Lt: "f32.lt",
    F32Gt: "f32.gt",
    F64Lt: "f64.lt",
    F64Gt: "f64.gt",
    I32Add: "i32.add",
    I32Sub: "i32.sub",
    I32Mul: "i32.mul",
    I32DivS: "i32.div_s",
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
}

pub struct Compiler<'a> {
    code: Vec<u8>,
    locals: Vec<VarDecl>,
    types: &'a mut Vec<FuncType>,
    imports: &'a [FuncImport],
    funcs: &'a mut Vec<FuncDef>,
}

impl<'a> Compiler<'a> {
    pub fn new(
        args: Vec<VarDecl>,
        types: &'a mut Vec<FuncType>,
        imports: &'a [FuncImport],
        funcs: &'a mut Vec<FuncDef>,
    ) -> Self {
        Self {
            code: vec![],
            locals: args,
            types,
            imports,
            funcs,
        }
    }

    pub fn compile(&mut self, ast: &[Statement]) -> Result<Type, String> {
        // let mut target_stack = vec![];
        let last = self.emit_stmts(ast)?;
        self.code.push(OpCode::End as u8);
        Ok(last)
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
            Expression::LiteralI32(num) => {
                self.code.push(OpCode::I32Const as u8);
                encode_sleb128(&mut self.code, *num).unwrap();
                Ok(Type::I32)
            }
            Expression::LiteralF64(num) => {
                self.code.push(OpCode::F64Const as u8);
                self.code.write_all(&num.to_le_bytes()).unwrap();
                Ok(Type::F64)
            }
            Expression::Variable(name) => {
                let (ret, local) = self
                    .locals
                    .iter()
                    .enumerate()
                    .find(|(_, local)| &local.name == name)
                    .ok_or_else(|| format!("Variable {name} not found"))?;
                let ty = local.ty;
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
                encode_leb128(&mut self.code, idx as i32).unwrap();
                let fn_ty = &self.types[fn_ty];
                Ok(fn_ty.results.get(0).copied().unwrap_or(Type::Void))
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
                        encode_leb128(&mut self.code, v as i32).unwrap();
                        self.code.push(OpCode::I32Sub as u8);
                        Ok(Type::I32)
                    }
                    Type::I64 => {
                        let v = self.add_local("", Type::I32);
                        self.code.push(OpCode::I32Const as u8);
                        encode_leb128(&mut self.code, 0).unwrap();
                        self.code.push(OpCode::LocalGet as u8);
                        encode_leb128(&mut self.code, v as i32).unwrap();
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
                },
            ),
            Expression::Lt(lhs, rhs) => self.emit_bin_op(
                lhs,
                rhs,
                "lt",
                TypeMap {
                    i32: OpCode::I32LtS,
                    i64: OpCode::I64LtS,
                    f32: OpCode::F32Lt,
                    f64: OpCode::F64Lt,
                },
            ),
            Expression::Gt(lhs, rhs) => self.emit_bin_op(
                lhs,
                rhs,
                "gt",
                TypeMap {
                    i32: OpCode::I32GtS,
                    i64: OpCode::I64GtS,
                    f32: OpCode::F32Gt,
                    f64: OpCode::F64Gt,
                },
            ),
            Expression::Conditional(cond, t_branch, f_branch) => {
                self.emit_expr(cond)?;
                self.code.push(OpCode::If as u8);
                let ty_fixup = self.code.len();
                self.code.push(Type::Void.code());

                let t_result = self.emit_stmts(t_branch)?;

                self.code[ty_fixup] = t_result.code();

                if let Some(f_branch) = f_branch {
                    self.code.push(OpCode::Else as u8);
                    let f_result = self.emit_stmts(f_branch)?;
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

    fn local_get(&mut self, idx: usize) {
        self.code.push(OpCode::LocalGet as u8);
        encode_leb128(&mut self.code, idx as i32).unwrap();
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
        let (op, ty) = match (lhs, rhs) {
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
            _ => return Err(format!("Type mismatch for {name:?}: {lhs} and {rhs}")),
        };
        self.code.push(op as u8);
        Ok(ty)
    }

    /// Returns if a value is pushed to the stack
    fn emit_stmt(&mut self, stmt: &Statement) -> Result<Type, String> {
        match stmt {
            Statement::Expr(ex) => self.emit_expr(ex),
            Statement::VarDecl(name, ty, ex) => {
                let ex_ty = self.emit_expr(ex)?;
                self.coerce_type(*ty, ex_ty).map_err(|_| {
                    format!(
                        "Variable declared type {ty:?} and initializer type {ex:?} are different"
                    )
                })?;
                self.add_local(*name, *ty);
                Ok(Type::Void)
            }
            Statement::VarAssign(name, ex) => {
                self.emit_expr(ex)?;
                let (idx, _) = self
                    .locals
                    .iter()
                    .enumerate()
                    .find(|(_, local)| &local.name == name)
                    .ok_or_else(|| format!("Variable {name} not found"))?;
                self.code.push(OpCode::LocalSet as u8);
                encode_leb128(&mut self.code, idx as i32).unwrap();
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
                self.coerce_type(Type::I32, start_ty)?; // For now for loop uses i32 for loop variable
                let idx = self.add_local(*name, Type::I32);

                let end_ty = self.emit_expr(end)?;
                self.coerce_type(Type::I32, end_ty)?; // Enough fors
                let end = self.add_local("", Type::I32);

                // Start block
                self.code.push(OpCode::Block as u8);
                self.code.push(Type::Void.code());

                // Start loop
                self.code.push(OpCode::Loop as u8);
                self.code.push(Type::Void.code());

                // End condition
                self.local_get(end);
                self.local_get(idx);
                self.code.push(OpCode::I32LtS as u8);
                self.code.push(OpCode::BrIf as u8);
                encode_leb128(&mut self.code, 1).unwrap();
                self.emit_stmts(stmts)?;

                // Incr idx
                self.local_get(idx);
                self.code.push(OpCode::I32Const as u8);
                encode_leb128(&mut self.code, 1).unwrap();
                self.code.push(OpCode::I32Add as u8);
                self.code.push(OpCode::LocalSet as u8);
                encode_leb128(&mut self.code, idx as i32).unwrap();

                self.code.push(OpCode::Br as u8);
                encode_leb128(&mut self.code, 0 as i32).unwrap();

                // end loop
                self.code.push(OpCode::End as u8);

                // end block
                self.code.push(OpCode::End as u8);

                Ok(Type::Void)
            }
            Statement::Brace(stmts) => self.emit_stmts(stmts),
            Statement::Return(ex) => {
                if let Some(ex) = ex {
                    self.emit_expr(ex)?;
                }
                self.code.push(OpCode::Return as u8);
                Ok(Type::Void)
            }
        }
    }

    fn emit_stmts(&mut self, stmts: &[Statement]) -> Result<Type, String> {
        let mut last_ty = Type::Void;
        for stmt in stmts {
            if last_ty != Type::Void {
                self.code.push(OpCode::Drop as u8);
            }
            last_ty = self.emit_stmt(stmt)?;
        }
        Ok(last_ty)
    }

    /// Add instructions to pop a value from the stack and puts it as a local variable.
    /// Returns the local index of added local variable.
    fn add_local(&mut self, name: impl Into<String>, ty: Type) -> usize {
        let ret = self.locals.len();
        self.code.push(OpCode::LocalSet as u8);
        encode_leb128(&mut self.code, ret as i32).unwrap();
        self.locals.push(VarDecl {
            name: name.into(),
            ty,
        });
        ret
    }

    fn coerce_type(&mut self, ty: Type, ex: Type) -> Result<(), String> {
        match (ty, ex) {
            (Type::I32, Type::I32)
            | (Type::I64, Type::I64)
            | (Type::F32, Type::F32)
            | (Type::F64, Type::F64) => {}
            (Type::I32, Type::F64) => self.code.push(OpCode::I32TruncF64S as u8),
            (Type::F64, Type::I32) => self.code.push(OpCode::F64ConvertI32S as u8),
            _ => return Err(format!("Coercing type {ty:?} from type {ex:?} failed")),
        }
        Ok(())
    }
}

pub(crate) fn encode_leb128(f: &mut impl Write, mut value: i32) -> std::io::Result<()> {
    loop {
        let mut byte = (value & 0x7f) as u8;
        value >>= 7;
        if value != 0 {
            // set high-order bit of byte;
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

pub(crate) fn encode_sleb128(f: &mut impl Write, mut value: i32) -> std::io::Result<()> {
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
            I32Const => {
                let arg = decode_sleb128(&mut cur)?;
                writeln!(f, "{indent}i32.const {arg}")?;
            }
            F64Const => {
                let mut buf = [0u8; std::mem::size_of::<f64>()];
                cur.read_exact(&mut buf)?;
                let arg = f64::from_le_bytes(buf);
                writeln!(f, "{indent}f64.const {arg}")?;
            }
            I32LtS | I32LtU | I32GtS | I32GtU | I32Add | I32Sub | I32Mul | I32DivS | I64LtS
            | I64LtU | I64GtS | I64GtU | I64Add | I64Sub | I64Mul | I64DivS | F32Neg | F32Lt
            | F32Gt | F32Add | F32Sub | F32Mul | F32Div | F64Neg | F64Lt | F64Gt | F64Add
            | F64Sub | F64Mul | F64Div | I32WrapI64 | I32TruncF32S | I32TruncF64S
            | I64ExtendI32S | I64TruncF32S | I64TruncF64S | F32ConvertI32S | F32ConvertI64S
            | F32DemoteF64 | F64ConvertI32S | F64ConvertI64S | F64PromoteF32 => {
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
        func_ty.results[0]
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
