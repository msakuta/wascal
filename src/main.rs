mod compiler;
mod parser;

use std::io::Write;

use compiler::Compiler;
use parser::add;

const WASM_BINARY_VERSION: [u8; 4] = [1, 0, 0, 0];
const WASM_TYPE_SECTION: u8 = 1;
const WASM_FUNCTION_SECTION: u8 = 3;
const WASM_CODE_SECTION: u8 = 0x0a;
const WASM_EXPORT_SECTION: u8 = 0x07;

enum Type {
    I32,
    I64,
}

impl Type {
    fn code(&self) -> u8 {
        match self {
            Self::I32 => 0x7f,
            Self::I64 => 0x7e,
        }
    }
}

#[repr(C)]
enum OpCode {
    I32Const = 0x41,
    LocalGet = 0x20,
    LocalSet = 0x21,
    I32Add = 0x6a,
    End = 0x0b,
}

struct FuncType {
    params: Vec<Type>,
    results: Vec<Type>,
}

struct FuncDef {
    name: &'static str,
    ty: usize,
    code: Vec<u8>,
    locals: usize,
}

fn main() -> std::io::Result<()> {
    let mut f = std::io::BufWriter::new(std::fs::File::create("wastack.wasm")?);
    f.write_all(b"\0asm")?;
    f.write_all(&WASM_BINARY_VERSION)?;

    let types = [FuncType {
        params: vec![],
        results: vec![Type::I32],
    }];

    let arg = std::env::args().nth(1).unwrap_or("42+3".to_string());

    let (_, ast) = add(&arg).unwrap();

    let mut compiler = Compiler::new();
    compiler.compile(&ast);

    let funcs = vec![FuncDef {
        name: "hello",
        ty: 0,
        code: compiler.get_code().to_vec(),
        locals: compiler.get_locals(),
        // code: vec![OpCode::I32Const as u8, 0x2B, OpCode::End as u8],
    }];

    write_section(&mut f, WASM_TYPE_SECTION, &types_section(&types)?)?;

    write_section(&mut f, WASM_FUNCTION_SECTION, &functions_section(&funcs)?)?;

    write_section(&mut f, WASM_EXPORT_SECTION, &export_section(&funcs)?)?;

    write_section(&mut f, WASM_CODE_SECTION, &code_section(&funcs)?)?;

    Ok(())
}

fn write_section(f: &mut impl Write, section: u8, payload: &[u8]) -> std::io::Result<()> {
    f.write_all(&[section])?;
    f.write_all(&[payload.len() as u8])?;
    f.write_all(&payload)?;
    Ok(())
}

fn types_section(types: &[FuncType]) -> std::io::Result<Vec<u8>> {
    let mut types_buf: Vec<u8> = vec![];
    let mut types_writer = std::io::Cursor::new(&mut types_buf);

    types_writer.write_all(&[types.len() as u8])?; //size

    for ty in types {
        types_writer.write_all(&[0x60])?; //fn
        types_writer.write_all(&[ty.params.len() as u8])?;
        for param in &ty.params {
            types_writer.write_all(&[param.code()])?; // i32
        }
        types_writer.write_all(&[ty.results.len() as u8])?;
        for res in &ty.results {
            types_writer.write_all(&[res.code()])?; // i32
        }
    }

    Ok(types_buf)
}

fn functions_section(funcs: &[FuncDef]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];
    let mut writer = std::io::Cursor::new(&mut buf);

    writer.write_all(&[funcs.len() as u8])?;

    for fun in funcs {
        writer.write_all(&[fun.ty as u8])?;
    }

    Ok(buf)
}

fn code_section(funcs: &[FuncDef]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];
    let mut writer = std::io::Cursor::new(&mut buf);

    writer.write_all(&[funcs.len() as u8])?;

    for fun in funcs {
        let fun_buf = code_single(fun)?;
        writer.write_all(&[fun_buf.len() as u8])?;
        writer.write_all(&fun_buf)?;
    }

    Ok(buf)
}

fn code_single(fun: &FuncDef) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];
    let mut writer = std::io::Cursor::new(&mut buf);

    // for _local in 0..fun.locals {
    writer.write_all(&[1])?; // local decl count
    writer.write_all(&[fun.locals as u8])?;
    writer.write_all(&[Type::I32.code()])?;
    // }
    writer.write_all(&fun.code)?;

    Ok(buf)
}

fn export_section(funcs: &[FuncDef]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];
    let mut writer = std::io::Cursor::new(&mut buf);

    writer.write_all(&[funcs.len() as u8])?;

    for (i, fun) in funcs.iter().enumerate() {
        write_string(&mut writer, fun.name)?;
        writer.write_all(&[0 as u8])?; //export kind
        writer.write_all(&[i as u8])?;
    }

    Ok(buf)
}

fn write_string(f: &mut impl Write, s: &str) -> std::io::Result<()> {
    f.write_all(&[s.len() as u8])?;
    f.write_all(s.as_bytes())?;
    Ok(())
}
