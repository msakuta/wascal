use std::io::{Read, Write};

use crate::{
    parser::{Expression, FnDecl, For, Statement, VarDecl},
    FuncDef, FuncImport, FuncType, Type,
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
    Call = 0x10,
    Drop = 0x1a,
    LocalGet = 0x20,
    LocalSet = 0x21,
    I32Const = 0x41,
    F64Const = 0x44,
    I32LtS = 0x48,
    I32LtU = 0x49,
    I64LtS = 0x53,
    I64LtU = 0x54,
    F32Lt = 0x5d,
    F64Lt = 0x63,
    I32Add = 0x6a,
    I32Sub = 0x6b,
    I32Mul = 0x6c,
    I32DivS = 0x6d,
    I64Add = 0x7c,
    I64Sub = 0x7d,
    I64Mul = 0x7e,
    I64DivS = 0x7f,
    F32Add = 0x92,
    F32Sub = 0x93,
    F32Mul = 0x94,
    F32Div = 0x95,
    F64Add = 0xa0,
    F64Sub = 0xa1,
    F64Mul = 0xa2,
    F64Div = 0xa3,
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
    Block: "block", Loop: "loop", If: "if", Else: "else", End: "end", Br: "br", BrIf: "br_if", Call: "call", Drop: "drop",
    LocalGet: "local.get", LocalSet: "local.set", I32Const: "i32.const", F64Const: "f64.const",
    I32LtS: "i32.lt_s", I32LtU: "i32.lt_u", I64LtS: "i64.lt_s", I64LtU: "i64.lt_u", F32Lt: "f32.lt",
    F64Lt: "f64.lt", I32Add: "i32.add", I32Sub: "i32.sub", I32Mul: "i32.mul", I32DivS: "i32.div_s", I64Add: "i64.add", I64Sub: "i64.sub",
    I64Mul: "i64.mul", I64DivS: "i64.div_s", F32Add: "f32.add", F32Sub: "f32.sub", F32Mul: "f32.mul", F32Div: "f32.div",
    F64Add: "f64.add", F64Sub: "f64.sub", F64Mul: "f64.mul", F64Div: "f64.div",
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

    pub fn compile(&mut self, ast: &[Statement]) -> Result<bool, String> {
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
                // target_stack.push(());
                self.code.push(OpCode::I32Const as u8);
                encode_leb128(&mut self.code, *num).unwrap();
                Ok(Type::I32)
            }
            Expression::LiteralF64(num) => {
                // target_stack.push(());
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
            Expression::FnInvoke(name, arg) => {
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
                // We assume functions take exactly 1 argument and 1 return value.
                self.emit_expr(arg)?;
                self.code.push(OpCode::Call as u8);
                encode_leb128(&mut self.code, idx as i32).unwrap();
                let fn_ty = &self.types[fn_ty];
                Ok(fn_ty.results[0])
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
            Expression::Conditional(cond, t_branch, f_branch) => {
                self.emit_expr(cond)?;
                self.code.push(OpCode::If as u8);
                let ty_fixup = self.code.len();
                self.code.push(Type::Void.code());

                let t_branch = self.emit_stmts(t_branch)?;

                if t_branch {
                    self.code[ty_fixup] = Type::I32.code();
                }

                if let Some(f_branch) = f_branch {
                    self.code.push(OpCode::Else as u8);

                    if !self.emit_stmts(f_branch)? {
                        return Err("True branch with yielded value requires false branch to yield a value too".to_string());
                    }
                } else if t_branch {
                    return Err("True branch with yielded value requires false branch".to_string());
                }

                self.code.push(OpCode::End as u8);

                Ok(Type::I32)
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
        let op = match (lhs, rhs) {
            (Type::I32, Type::I32) => ty_map.i32,
            (Type::I64, Type::I64) => ty_map.i64,
            (Type::F32, Type::F32) => ty_map.f32,
            (Type::F64, Type::F64) => ty_map.f64,
            _ => return Err(format!("Type mismatch for {name:?}: {lhs} and {rhs}")),
        };
        self.code.push(op as u8);
        Ok(lhs)
    }

    /// Returns if a value is pushed to the stack
    fn emit_stmt(&mut self, stmt: &Statement) -> Result<bool, String> {
        match stmt {
            Statement::Expr(ex) => {
                self.emit_expr(ex)?;
                Ok(true)
            }
            Statement::VarDecl(name, ex) => {
                let ty = self.emit_expr(ex)?;
                self.add_local(*name, ty);
                Ok(false)
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
                Ok(false)
            }
            Statement::FnDecl(FnDecl {
                name,
                params,
                stmts,
                ret_ty,
            }) => {
                let mut compiler =
                    Compiler::new(params.clone(), self.types, self.imports, self.funcs);
                compiler.compile(stmts)?;
                let code = compiler.get_code().to_vec();
                let locals = compiler.get_locals().to_vec();
                let fn_def = FuncDef {
                    name: name.to_string(),
                    ty: self.types.len(),
                    code,
                    locals,
                };
                self.types.push(FuncType {
                    params: params.iter().map(|_| Type::I32).collect(),
                    results: vec![*ret_ty],
                });
                self.funcs.push(fn_def);
                Ok(false)
            }
            Statement::For(For {
                name,
                start,
                end,
                stmts,
            }) => {
                let start_ty = self.emit_expr(start)?;
                let idx = self.add_local(*name, start_ty);

                let end_ty = self.emit_expr(end)?;
                let end = self.add_local("", end_ty);

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

                Ok(false)
            }
            Statement::Brace(stmts) => self.emit_stmts(stmts),
        }
    }

    fn emit_stmts(&mut self, stmts: &[Statement]) -> Result<bool, String> {
        let mut has_value = false;
        for stmt in stmts {
            if has_value {
                self.code.push(OpCode::Drop as u8);
            }
            has_value = self.emit_stmt(stmt)?;
        }
        Ok(has_value)
    }

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
                let arg = decode_leb128(&mut cur)?;
                writeln!(f, "{indent}i32.const {arg}")?;
            }
            F64Const => {
                let mut buf = [0u8; std::mem::size_of::<f64>()];
                cur.read_exact(&mut buf)?;
                let arg = f64::from_le_bytes(buf);
                writeln!(f, "{indent}f64.const {arg}")?;
            }
            I32LtS | I32LtU | I32Add | I32Sub | I32Mul | I32DivS | I64LtS | I64LtU | I64Add
            | I64Sub | I64Mul | I64DivS | F32Lt | F32Add | F32Sub | F32Mul | F32Div | F64Lt
            | F64Add | F64Sub | F64Mul | F64Div => {
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
