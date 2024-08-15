mod compiler;
mod parser;

use std::io::Write;

use compiler::{disasm, encode_leb128, Compiler};
use parser::{parse, FnDecl, Statement, VarDecl};

const WASM_BINARY_VERSION: [u8; 4] = [1, 0, 0, 0];
const WASM_TYPE_SECTION: u8 = 1;
const WASM_IMPORT_SECTION: u8 = 0x2;
const WASM_FUNCTION_SECTION: u8 = 3;
const WASM_CODE_SECTION: u8 = 0x0a;
const WASM_EXPORT_SECTION: u8 = 0x07;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Type {
    I32,
    I64,
    F32,
    F64,
    // Pseudo type representing no value
    Void,
}

impl Type {
    fn code(&self) -> u8 {
        match self {
            Self::I32 => 0x7f,
            Self::I64 => 0x7e,
            Self::F32 => 0x7d,
            Self::F64 => 0x7c,
            Self::Void => 0x40,
        }
    }
}

impl From<u8> for Type {
    fn from(value: u8) -> Self {
        match value {
            0x7f => Self::I32,
            0x7e => Self::I64,
            0x7d => Self::F32,
            0x7c => Self::F64,
            0x40 => Self::Void,
            _ => panic!("Unknown type"),
        }
    }
}

impl TryFrom<&str> for Type {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Ok(match value {
            "i32" => Type::I32,
            "i64" => Type::I64,
            "f32" => Type::F32,
            "f64" => Type::F64,
            _ => return Err(format!("Unknown type {}", value)),
        })
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
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
    locals: Vec<VarDecl>,
}

fn main() -> std::io::Result<()> {
    let mut f = std::io::BufWriter::new(std::fs::File::create("wascal.wasm")?);
    f.write_all(b"\0asm")?;
    f.write_all(&WASM_BINARY_VERSION)?;

    let mut types = vec![FuncType {
        params: vec![Type::I32],
        results: vec![Type::I32],
    }];

    let arg = std::env::args()
        .nth(1)
        .unwrap_or("scripts/hello.wscl".to_string());

    let source = std::fs::read_to_string(arg)?;

    let stmts = parse(&source).unwrap();

    println!("ast: {stmts:?}");

    let imports = vec![
        FuncImport {
            module: "Math",
            name: "abs",
            ty: 0,
        },
        FuncImport {
            module: "console",
            name: "log",
            ty: 0,
        },
        FuncImport {
            module: "output",
            name: "putc",
            ty: 0,
        },
    ];

    let mut funcs = vec![];

    fn find_funcs<'a>(stmts: &'a [Statement<'a>], funcs: &mut Vec<&FnDecl<'a>>) {
        for stmt in stmts.iter() {
            if let Statement::FnDecl(fn_decl) = stmt {
                funcs.push(fn_decl);
                find_funcs(&fn_decl.stmts, funcs);
            }
            if let Statement::Brace(stmts) = stmt {
                find_funcs(stmts, funcs);
            }
        }
    }

    let mut func_stmts = vec![];

    find_funcs(&stmts, &mut func_stmts);

    println!("func_stmts: {}", func_stmts.len());

    for func_stmt in &func_stmts {
        let ty = types.len();
        types.push(FuncType {
            params: func_stmt.params.iter().map(|param| param.ty).collect(),
            results: vec![func_stmt.ret_ty],
        });
        let func = FuncDef {
            name: func_stmt.name.to_string(),
            ty,
            code: vec![],
            locals: vec![],
        };

        funcs.push(func);
    }

    for (i, func_stmt) in func_stmts.iter().enumerate() {
        let mut compiler = Compiler::new(
            func_stmt
                .params
                .iter()
                .map(|param| param.clone())
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
        let locals = compiler.get_locals().to_vec();

        let func = &mut funcs[i];
        func.code = code;
        func.locals = locals;

        let params = &func.locals[..types[func.ty].params.len()];
        let params = params.iter().fold("".to_string(), |mut acc, cur| {
            if !acc.is_empty() {
                acc += ", ";
            }
            acc += &format!("{}: {}", cur.name, cur.ty);
            acc
        });

        println!(
            "Disasm {}({}) -> {}: ",
            func.name, params, types[func.ty].results[0]
        );
        disasm(&func.code, &mut std::io::stdout())?;
    }

    write_section(&mut f, WASM_TYPE_SECTION, &types_section(&types)?)?;

    write_section(&mut f, WASM_IMPORT_SECTION, &import_section(&imports)?)?;

    write_section(&mut f, WASM_FUNCTION_SECTION, &functions_section(&funcs)?)?;

    write_section(
        &mut f,
        WASM_EXPORT_SECTION,
        &export_section(&funcs, &imports)?,
    )?;

    write_section(&mut f, WASM_CODE_SECTION, &code_section(&funcs, &types)?)?;

    Ok(())
}

fn write_section(f: &mut impl Write, section: u8, payload: &[u8]) -> std::io::Result<()> {
    f.write_all(&[section])?;
    encode_leb128(f, payload.len() as i32)?;
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
            types_writer.write_all(&[param.code()])?;
        }
        types_writer.write_all(&[ty.results.len() as u8])?;
        for res in &ty.results {
            types_writer.write_all(&[res.code()])?;
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

fn code_section(funcs: &[FuncDef], types: &[FuncType]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];

    buf.write_all(&[funcs.len() as u8])?;

    for fun in funcs {
        let fun_buf = code_single(fun, types)?;
        encode_leb128(&mut buf, fun_buf.len() as i32)?;
        buf.write_all(&fun_buf)?;
    }

    Ok(buf)
}

fn code_single(fun: &FuncDef, types: &[FuncType]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];

    let fn_type = &types[fun.ty];

    let mut last = None;
    let mut chunks = 0;
    let mut run_length = 0;
    for local in fun.locals.iter().skip(fn_type.params.len()) {
        if Some(local.ty) == last {
            run_length += 1;
        } else {
            if let Some(last) = last {
                if 0 < run_length {
                    encode_leb128(&mut buf, run_length)?;
                    buf.push(last.code());
                    chunks += 1;
                }
            }
            run_length = 1;
            last = Some(local.ty);
        }
    }

    if let Some(last) = last {
        if 0 < run_length {
            encode_leb128(&mut buf, run_length)?;
            buf.push(last.code());
            chunks += 1;
        }
    }

    let mut ret = vec![];
    encode_leb128(&mut ret, chunks)?;
    ret.write_all(&buf)?;

    ret.write_all(&fun.code)?;

    Ok(ret)
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
