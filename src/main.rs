mod compiler;
mod parser;

use std::io::Write;

use compiler::{disasm, encode_leb128, Compiler};
use parser::{parse, Statement};

const WASM_BINARY_VERSION: [u8; 4] = [1, 0, 0, 0];
const WASM_TYPE_SECTION: u8 = 1;
const WASM_IMPORT_SECTION: u8 = 0x2;
const WASM_FUNCTION_SECTION: u8 = 3;
const WASM_CODE_SECTION: u8 = 0x0a;
const WASM_EXPORT_SECTION: u8 = 0x07;

#[derive(Debug)]
enum Type {
    I32,
    I64,
    // Pseudo type representing no value
    Void,
}

impl Type {
    fn code(&self) -> u8 {
        match self {
            Self::I32 => 0x7f,
            Self::I64 => 0x7e,
            Self::Void => 0x40,
        }
    }
}

impl From<u8> for Type {
    fn from(value: u8) -> Self {
        match value {
            0x7f => Self::I32,
            0x7e => Self::I64,
            0x40 => Self::Void,
            _ => panic!("Unknown type"),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::Void => write!(f, "void"),
        }
    }
}

struct FuncType {
    params: Vec<Type>,
    results: Vec<Type>,
}

struct FuncImport {
    module: &'static str,
    name: &'static str,
    ty: usize,
}

struct FuncDef {
    name: String,
    ty: usize,
    code: Vec<u8>,
    locals: usize,
}

fn main() -> std::io::Result<()> {
    let mut f = std::io::BufWriter::new(std::fs::File::create("wascal.wasm")?);
    f.write_all(b"\0asm")?;
    f.write_all(&WASM_BINARY_VERSION)?;

    let mut types = vec![
        FuncType {
            params: vec![Type::I32],
            results: vec![Type::I32],
        },
        FuncType {
            params: vec![Type::I32, Type::I32],
            results: vec![Type::I32],
        },
    ];

    let arg = std::env::args()
        .nth(1)
        .unwrap_or("scripts/hello.wscl".to_string());

    let source = std::fs::read_to_string(arg)?;

    let stmts = parse(&source).unwrap();

    println!("ast: {stmts:?}");

    let imports = vec![FuncImport {
        module: "Math",
        name: "abs",
        ty: 0,
    }];

    let mut funcs = vec![];

    let func_stmts = stmts
        .iter()
        .enumerate()
        .filter_map(|(_, f)| {
            if let Statement::FnDecl(fn_decl) = f {
                Some(fn_decl)
            } else {
                None
            }
        })
        .collect::<Vec<_>>();

    println!("func_stmts: {}", func_stmts.len());

    for func_stmt in &func_stmts {
        let mut compiler = Compiler::new(
            func_stmt
                .params
                .iter()
                .map(|param| param.to_string())
                .collect::<Vec<_>>(),
            &mut types,
            &imports,
            &mut funcs,
        );
        if let Err(e) = compiler.compile(&func_stmt.stmts) {
            println!("Compile error: {e}");
            return Ok(());
        }

        let code = compiler.get_code().to_vec();
        let locals = compiler.get_locals().len();

        let ty = types.len();
        types.push(FuncType {
            params: func_stmt.params.iter().map(|_| Type::I32).collect(),
            results: vec![Type::I32],
        });
        let func = FuncDef {
            name: func_stmt.name.to_string(),
            ty,
            code,
            locals,
        };

        println!("Disasm {}: ", func.name);
        disasm(&func.code, &mut std::io::stdout())?;

        funcs.push(func);
    }

    write_section(&mut f, WASM_TYPE_SECTION, &types_section(&types)?)?;

    write_section(&mut f, WASM_IMPORT_SECTION, &import_section(&imports)?)?;

    write_section(&mut f, WASM_FUNCTION_SECTION, &functions_section(&funcs)?)?;

    write_section(
        &mut f,
        WASM_EXPORT_SECTION,
        &export_section(&funcs, &imports)?,
    )?;

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

fn import_section(funcs: &[FuncImport]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];
    let mut writer = &mut buf;

    encode_leb128(writer, funcs.len() as i32)?;

    for fun in funcs.iter() {
        write_string(&mut writer, fun.module)?;
        write_string(&mut writer, fun.name)?;
        writer.write_all(&[0 as u8])?; // import kind
        writer.write_all(&[fun.ty as u8])?;
    }

    Ok(buf)
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

fn export_section(funcs: &[FuncDef], imports: &[FuncImport]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];
    let mut writer = std::io::Cursor::new(&mut buf);

    encode_leb128(&mut writer, funcs.len() as i32)?;

    for (i, fun) in funcs.iter().enumerate() {
        write_string(&mut writer, &fun.name)?;
        encode_leb128(&mut writer, 0)?; //export kind
        encode_leb128(&mut writer, (i + imports.len()) as i32)?;
    }

    Ok(buf)
}

fn write_string(f: &mut impl Write, s: &str) -> std::io::Result<()> {
    encode_leb128(f, s.len() as i32)?;
    f.write_all(s.as_bytes())?;
    Ok(())
}
