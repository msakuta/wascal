//! Code to write wasm file format sections.
use crate::{
    compiler::{disasm_func, encode_leb128, Compiler},
    infer::{run_type_infer, set_infer_debug},
    model::{FuncDef, FuncImport, FuncType},
    parser::{parse, FnDecl, Statement},
    Type,
};
use std::{error::Error, io::Write};

const WASM_BINARY_VERSION: [u8; 4] = [1, 0, 0, 0];
const WASM_TYPE_SECTION: u8 = 1;
const WASM_IMPORT_SECTION: u8 = 0x2;
const WASM_FUNCTION_SECTION: u8 = 3;
const WASM_CODE_SECTION: u8 = 0x0a;
const WASM_EXPORT_SECTION: u8 = 0x07;

#[derive(Debug)]
pub enum CompileError {
    IO(std::io::Error),
    Compile(String),
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::IO(e) => e.fmt(f),
            Self::Compile(e) => write!(f, "{e}"),
        }
    }
}

impl Error for CompileError {}

impl From<std::io::Error> for CompileError {
    fn from(value: std::io::Error) -> Self {
        Self::IO(value)
    }
}

pub type CompileResult<T> = Result<T, CompileError>;

pub fn compile_wasm(
    f: &mut impl Write,
    source: &str,
    types: &mut Vec<FuncType>,
    imports: &[FuncImport],
    disasm_f: Option<&mut dyn Write>,
    typeinf_f: Option<&mut dyn Write>,
    debug_type_infer: bool,
) -> CompileResult<()> {
    f.write_all(b"\0asm")?;
    f.write_all(&WASM_BINARY_VERSION)?;

    let funcs = codegen(
        source,
        types,
        imports,
        disasm_f,
        typeinf_f,
        debug_type_infer,
    )?;

    write_section(f, WASM_TYPE_SECTION, &types_section(&types)?)?;

    write_section(f, WASM_IMPORT_SECTION, &import_section(&imports)?)?;

    write_section(f, WASM_FUNCTION_SECTION, &functions_section(&funcs)?)?;

    write_section(f, WASM_EXPORT_SECTION, &export_section(&funcs, &imports)?)?;

    write_section(f, WASM_CODE_SECTION, &code_section(&funcs, &types)?)?;

    Ok(())
}

#[allow(dead_code)]
pub fn disasm_wasm(
    f: &mut impl Write,
    source: &str,
    types: &mut Vec<FuncType>,
    imports: &[FuncImport],
) -> CompileResult<()> {
    let _ = codegen(source, types, imports, Some(f), None, false)?;
    Ok(())
}

#[allow(dead_code)]
pub fn typeinf_wasm(
    f: &mut impl Write,
    source: &str,
    types: &mut Vec<FuncType>,
    imports: &[FuncImport],
) -> CompileResult<()> {
    let _ = codegen(source, types, imports, None, Some(f), false)?;
    Ok(())
}

fn codegen(
    source: &str,
    types: &mut Vec<FuncType>,
    imports: &[FuncImport],
    mut disasm_f: Option<&mut dyn Write>,
    typeinf_f: Option<&mut dyn Write>,
    debug_type_infer: bool,
) -> CompileResult<Vec<FuncDef>> {
    let mut stmts = parse(&source).map_err(|e| CompileError::Compile(e))?;

    set_infer_debug(debug_type_infer);

    run_type_infer(&mut stmts, types, imports, typeinf_f)?;

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
        let ret_ty = func_stmt.ret_ty.determine().ok_or_else(|| {
            CompileError::Compile(format!(
                "Function return type could not be determined: {}",
                func_stmt.ret_ty
            ))
        })?;
        types.push(FuncType {
            params: func_stmt
                .params
                .iter()
                .map(|param| param.ty.determine())
                .collect::<Option<Vec<_>>>()
                .ok_or_else(|| {
                    CompileError::Compile(
                        "Function argument type could not be determined".to_string(),
                    )
                })?,
            results: if ret_ty != Type::Void {
                vec![ret_ty]
            } else {
                vec![]
            },
        });
        let func = FuncDef {
            name: func_stmt.name.to_string(),
            ty,
            code: vec![],
            locals: vec![],
            public: func_stmt.public,
        };

        funcs.push(func);
    }

    for (i, func_stmt) in func_stmts.iter().enumerate() {
        let ret_ty = func_stmt.ret_ty.determine().ok_or_else(|| {
            CompileError::Compile("Could not determine return type by type inference".to_string())
        })?;
        let mut compiler = Compiler::new(
            func_stmt
                .params
                .iter()
                .map(|param| param.clone())
                .collect::<Vec<_>>(),
            ret_ty,
            types,
            &imports,
            &mut funcs,
        );
        if let Err(e) = compiler.compile(&func_stmt.stmts, ret_ty) {
            return Err(CompileError::Compile(format!(
                "Error in compiling function {}: {e}",
                func_stmt.name
            )));
        }

        let code = compiler.get_code().to_vec();
        let locals = compiler.get_locals().to_vec();

        let func = &mut funcs[i];
        func.code = code;
        func.locals = locals;

        if let Some(ref mut disasm_f) = disasm_f {
            let func_ty = &types[func.ty];

            disasm_func(&func, &func_ty, disasm_f)?;
        }
    }

    Ok(funcs)
}

fn write_section(f: &mut impl Write, section: u8, payload: &[u8]) -> std::io::Result<()> {
    f.write_all(&[section])?;
    encode_leb128(f, payload.len() as i32)?;
    f.write_all(&payload)?;
    Ok(())
}

fn types_section(types: &[FuncType]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];

    encode_leb128(&mut buf, types.len() as i32)?; //size

    for ty in types {
        buf.write_all(&[0x60])?; //fn
        encode_leb128(&mut buf, ty.params.len() as i32)?;
        for param in &ty.params {
            buf.write_all(&[param.code()])?;
        }
        encode_leb128(&mut buf, ty.results.len() as i32)?;
        for res in &ty.results {
            buf.write_all(&[res.code()])?;
        }
    }

    Ok(buf)
}

fn import_section(funcs: &[FuncImport]) -> std::io::Result<Vec<u8>> {
    let mut buf: Vec<u8> = vec![];
    let mut writer = &mut buf;

    encode_leb128(writer, funcs.len() as i32)?;

    for fun in funcs.iter() {
        write_string(&mut writer, &fun.module)?;
        write_string(&mut writer, &fun.name)?;
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
                    buf.push(
                        last.determine()
                            .ok_or_else(|| {
                                std::io::Error::other(
                                    "Param type could not be determined".to_string(),
                                )
                            })?
                            .code(),
                    );
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
            buf.push(
                last.determine()
                    .ok_or_else(|| {
                        std::io::Error::other("Return type could not be determined".to_string())
                    })?
                    .code(),
            );
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

    encode_leb128(
        &mut writer,
        funcs.iter().filter(|f| f.public).count() as i32,
    )?;

    for (i, fun) in funcs.iter().enumerate().filter(|(_, f)| f.public) {
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
