use std::io::{Read, Write};

use crate::{
    parser::{Expression, FnDecl, Statement},
    FuncDef, FuncImport, FuncType, Type,
};

#[repr(C)]
pub(crate) enum OpCode {
    If = 0x04,
    Else = 0x05,
    End = 0x0b,
    Call = 0x10,
    LocalGet = 0x20,
    LocalSet = 0x21,
    I32Const = 0x41,
    I32Lts = 0x48,
    I32Ltu = 0x49,
    I32Add = 0x6a,
    I32Sub = 0x6b,
    I32Mul = 0x6c,
    I32Div = 0x6d,
}

macro_rules! impl_op_from {
    ($($op:ident,)*) => {
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
    }
}

impl_op_from!(
    If, Else, End, Call, LocalGet, LocalSet, I32Const, I32Lts, I32Ltu, I32Add, I32Sub, I32Mul,
    I32Div,
);

pub struct Compiler<'a> {
    code: Vec<u8>,
    target_stack: usize,
    locals: Vec<String>,
    types: &'a mut Vec<FuncType>,
    imports: &'a [FuncImport],
    funcs: &'a mut Vec<FuncDef>,
}

impl<'a> Compiler<'a> {
    pub fn new(
        args: Vec<String>,
        types: &'a mut Vec<FuncType>,
        imports: &'a [FuncImport],
        funcs: &'a mut Vec<FuncDef>,
    ) -> Self {
        Self {
            code: vec![],
            target_stack: 0,
            locals: args,
            types,
            imports,
            funcs,
        }
    }

    pub fn compile(&mut self, ast: &[Statement]) -> Result<(), String> {
        // let mut target_stack = vec![];
        let last = self.emit_stmts(ast)?;
        if let Some(last) = last {
            self.code
                .extend_from_slice(&[OpCode::LocalGet as u8, last as u8]);
        }
        self.code.push(OpCode::End as u8);
        Ok(())
    }

    pub fn get_code(&self) -> &[u8] {
        &self.code
    }

    pub fn get_locals(&self) -> &[String] {
        &self.locals
    }

    fn emit_expr(&mut self, ast: &Expression) -> Result<usize, String> {
        match ast {
            Expression::Literal(num) => {
                // target_stack.push(());
                self.code.push(OpCode::I32Const as u8);
                encode_leb128(&mut self.code, *num).unwrap();
                self.code.push(OpCode::LocalSet as u8);
                encode_leb128(&mut self.code, self.locals.len() as i32).unwrap();
                let ret = self.locals.len();
                self.locals.push("".to_string());
                Ok(ret)
            }
            Expression::Variable(name) => {
                let (ret, _) = self
                    .locals
                    .iter()
                    .enumerate()
                    .find(|(_, local)| local == name)
                    .ok_or_else(|| format!("Variable {name} not found"))?;
                Ok(ret)
            }
            Expression::FnInvoke(name, arg) => {
                let Some((i, _)) = self.funcs.iter().enumerate().find(|(_, f)| f.name == *name)
                else {
                    return Err(format!("Calling undefined function {}", name));
                };
                let arg = self.emit_expr(arg)?;
                self.code.push(OpCode::LocalGet as u8);
                encode_leb128(&mut self.code, arg as i32).unwrap();
                self.code.push(OpCode::Call as u8);
                encode_leb128(&mut self.code, (i + self.imports.len()) as i32).unwrap();
                self.code.push(OpCode::LocalSet as u8);
                let ret = self.locals.len();
                encode_leb128(&mut self.code, self.locals.len() as i32).unwrap();
                self.locals.push("".to_string());
                Ok(ret)
            }
            Expression::Add(lhs, rhs) => self.emit_bin_op(lhs, rhs, OpCode::I32Add),
            Expression::Sub(lhs, rhs) => self.emit_bin_op(lhs, rhs, OpCode::I32Sub),
            Expression::Mul(lhs, rhs) => self.emit_bin_op(lhs, rhs, OpCode::I32Mul),
            Expression::Div(lhs, rhs) => self.emit_bin_op(lhs, rhs, OpCode::I32Div),
            Expression::Lt(lhs, rhs) => self.emit_bin_op(lhs, rhs, OpCode::I32Lts),
            Expression::Conditional(cond, t_branch, f_branch) => {
                let cond = self.emit_expr(cond)?;
                self.local_get(cond);
                self.code.push(OpCode::If as u8);
                self.code.push(Type::Void.code());

                let t_branch = self
                    .emit_stmts(t_branch)?
                    .ok_or_else(|| "Code in true branch must yield a value".to_string())?;
                self.local_get(t_branch);
                let ret = self.locals.len();
                self.locals.push("".to_string());
                self.code.push(OpCode::LocalSet as u8);
                encode_leb128(&mut self.code, ret as i32).unwrap();

                if let Some(f_branch) = f_branch {
                    self.code.push(OpCode::Else as u8);

                    let f_branch = self
                        .emit_stmts(f_branch)?
                        .ok_or_else(|| "Code in false branch must yield a value".to_string())?;
                    self.local_get(f_branch);
                    self.locals.push("".to_string());
                    self.code.push(OpCode::LocalSet as u8);
                    encode_leb128(&mut self.code, ret as i32).unwrap();
                }

                self.code.push(OpCode::End as u8);

                Ok(ret)
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
        op: OpCode,
    ) -> Result<usize, String> {
        if let (Expression::Literal(lhs), Expression::Literal(rhs)) = (lhs, rhs) {
            self.code.push(OpCode::I32Const as u8);
            encode_leb128(&mut self.code, *lhs).unwrap();
            self.code.push(OpCode::I32Const as u8);
            encode_leb128(&mut self.code, *rhs).unwrap();
        } else {
            let lhs = self.emit_expr(lhs)?;
            let rhs = self.emit_expr(rhs)?;
            self.code
                .extend_from_slice(&[OpCode::LocalGet as u8, lhs as u8]);
            self.code
                .extend_from_slice(&[OpCode::LocalGet as u8, rhs as u8]);
        }
        self.code.push(op as u8);
        self.code.push(OpCode::LocalSet as u8);
        encode_leb128(&mut self.code, self.locals.len() as i32).unwrap();
        let ret = self.locals.len();
        self.locals.push("".to_string());
        Ok(ret)
    }

    fn emit_stmt(&mut self, stmt: &Statement) -> Result<Option<usize>, String> {
        match stmt {
            Statement::Expr(ex) => Ok(Some(self.emit_expr(ex)?)),
            Statement::VarDecl(name, ex) => {
                let val = self.emit_expr(ex)?;
                self.code
                    .extend_from_slice(&[OpCode::LocalGet as u8, val as u8]);
                let idx = self.locals.len();
                self.code.push(OpCode::LocalSet as u8);
                encode_leb128(&mut self.code, self.locals.len() as i32).unwrap();
                self.locals.push(name.to_string());
                Ok(Some(idx))
            }
            Statement::FnDecl(FnDecl {
                name,
                params,
                stmts,
            }) => {
                let mut compiler = Compiler::new(
                    params.iter().map(|v| v.to_string()).collect(),
                    self.types,
                    self.imports,
                    self.funcs,
                );
                compiler.compile(stmts)?;
                let code = compiler.get_code().to_vec();
                let locals = compiler.get_locals().len();
                let fn_def = FuncDef {
                    name: name.to_string(),
                    ty: self.types.len(),
                    code,
                    locals,
                };
                self.types.push(FuncType {
                    params: params.iter().map(|_| Type::I32).collect(),
                    results: vec![Type::I32],
                });
                self.funcs.push(fn_def);
                Ok(None)
            }
        }
    }

    fn emit_stmts(&mut self, stmts: &[Statement]) -> Result<Option<usize>, String> {
        let mut last = None;
        for stmt in stmts {
            if let Some(res) = self.emit_stmt(stmt)? {
                last = Some(res);
            }
        }
        Ok(last)
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
    let mut cur = std::io::Cursor::new(&code);
    let mut block_level = 1;
    loop {
        let mut op_code_buf = [0u8];
        cur.read_exact(&mut op_code_buf).unwrap();
        let op_code = op_code_buf[0];
        match OpCode::from(op_code) {
            If => {
                let mut ty = [0u8];
                cur.read_exact(&mut ty)?;
                writeln!(f, "  if (result {})", Type::from(ty[0]))?;
                block_level += 1;
            }
            Else => {
                writeln!(f, "  else")?;
            }
            Call => {
                let arg = decode_leb128(&mut cur)?;
                writeln!(f, "  call {arg}")?;
            }
            LocalGet => {
                let arg = decode_leb128(&mut cur)?;
                writeln!(f, "  local.get {arg}")?;
            }
            LocalSet => {
                let arg = decode_leb128(&mut cur)?;
                writeln!(f, "  local.set {arg}")?;
            }
            I32Const => {
                let arg = decode_leb128(&mut cur)?;
                writeln!(f, "  i32.const {arg}")?;
            }
            I32Lts => {
                writeln!(f, "  i32.lt_s")?;
            }
            I32Ltu => {
                writeln!(f, "  i32.lt_u")?;
            }
            I32Add => {
                writeln!(f, "  i32.add")?;
            }
            I32Sub => {
                writeln!(f, "  i32.sub")?;
            }
            I32Mul => {
                writeln!(f, "  i32.mul")?;
            }
            I32Div => {
                writeln!(f, "  i32.div")?;
            }
            End => {
                writeln!(f, "  end")?;
                block_level -= 1;
                if block_level == 0 {
                    return Ok(());
                };
            }
        }
    }
}
