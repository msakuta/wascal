use std::io::{Read, Write};

use crate::parser::{Expression, Statement};

#[repr(C)]
pub(crate) enum OpCode {
    Call = 0x10,
    LocalGet = 0x20,
    LocalSet = 0x21,
    I32Const = 0x41,
    I32Add = 0x6a,
    I32Mul = 0x6c,
    End = 0x0b,
}

pub struct Compiler {
    code: Vec<u8>,
    target_stack: usize,
    locals: Vec<String>,
}

impl Compiler {
    pub fn new(args: Vec<String>) -> Self {
        Self {
            code: vec![],
            target_stack: 0,
            locals: args,
        }
    }

    pub fn compile(&mut self, ast: &[Statement]) -> Result<(), String> {
        // let mut target_stack = vec![];
        let mut last = None;
        for stmt in ast {
            last = Some(self.emit_stmt(stmt)?);
        }
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

    pub fn disasm(&self, f: &mut impl Write) -> std::io::Result<()> {
        #![allow(non_upper_case_globals)]
        const Call: u8 = OpCode::Call as u8;
        const LocalGet: u8 = OpCode::LocalGet as u8;
        const LocalSet: u8 = OpCode::LocalSet as u8;
        const I32Const: u8 = OpCode::I32Const as u8;
        const I32Add: u8 = OpCode::I32Add as u8;
        const I32Mul: u8 = OpCode::I32Mul as u8;
        const End: u8 = OpCode::End as u8;
        let mut cur = std::io::Cursor::new(&self.code);
        loop {
            let mut op_code_buf = [0u8];
            cur.read_exact(&mut op_code_buf).unwrap();
            let op_code = op_code_buf[0];
            match op_code {
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
                I32Add => {
                    writeln!(f, "  i32.add")?;
                }
                I32Mul => {
                    writeln!(f, "  i32.mul")?;
                }
                End => return Ok(()),
                _ => panic!("code is broken: unknown opcode {op_code}"),
            }
        }
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
            Expression::FnInvoke(_name, arg) => {
                let arg = self.emit_expr(arg)?;
                self.code.push(OpCode::LocalGet as u8);
                encode_leb128(&mut self.code, arg as i32).unwrap();
                self.code.push(OpCode::Call as u8);
                encode_leb128(&mut self.code, 0).unwrap();
                self.code.push(OpCode::LocalSet as u8);
                let ret = self.locals.len();
                encode_leb128(&mut self.code, self.locals.len() as i32).unwrap();
                self.locals.push("".to_string());
                Ok(ret)
            }
            Expression::Add(lhs, rhs) => {
                if let (Expression::Literal(lhs), Expression::Literal(rhs)) = (&**lhs, &**rhs) {
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
                self.code.push(OpCode::I32Add as u8);
                self.code.push(OpCode::LocalSet as u8);
                encode_leb128(&mut self.code, self.locals.len() as i32).unwrap();
                let ret = self.locals.len();
                self.locals.push("".to_string());
                Ok(ret)
            }
            Expression::Mul(lhs, rhs) => {
                if let (Expression::Literal(lhs), Expression::Literal(rhs)) = (&**lhs, &**rhs) {
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
                self.code.push(OpCode::I32Mul as u8);
                self.code.push(OpCode::LocalSet as u8);
                encode_leb128(&mut self.code, self.locals.len() as i32).unwrap();
                let ret = self.locals.len();
                self.locals.push("".to_string());
                Ok(ret)
            }
        }
    }

    fn emit_stmt(&mut self, stmt: &Statement) -> Result<usize, String> {
        match stmt {
            Statement::Expr(ex) => self.emit_expr(ex),
            Statement::VarDecl(name, ex) => {
                let val = self.emit_expr(ex)?;
                self.code
                    .extend_from_slice(&[OpCode::LocalGet as u8, val as u8]);
                let idx = self.locals.len();
                self.code.push(OpCode::LocalSet as u8);
                encode_leb128(&mut self.code, self.locals.len() as i32).unwrap();
                self.locals.push(name.to_string());
                Ok(idx)
            }
        }
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
