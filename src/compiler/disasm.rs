use std::io::{Read, Write};

use super::{OpCode, decode_leb128, decode_sleb128};

use crate::{model::FuncDef, FuncType, Type};

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
