use wasm_bindgen::prelude::*;

use wascal::{compile_wasm, disasm_wasm, parse, FuncImport, FuncType, Type};

use std::io::Write;

#[wasm_bindgen]
pub fn parse_ast(source: &str) -> Result<String, JsValue> {
    let ast = parse(source).map_err(JsValue::from)?;
    Ok(format!("{ast:#?}"))
}

#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Vec<u8>, JsValue> {
    let (mut types, imports) = default_imports();
    let mut buf = vec![];
    compile_wasm(&mut buf, source, &mut types, &imports, None)
        .map_err(|e| JsValue::from(format!("{e}")))?;
    Ok(buf)
}

#[wasm_bindgen]
pub fn disasm(source: &str) -> Result<String, JsValue> {
    let (mut types, imports) = default_imports();
    let mut buf = vec![];
    disasm_wasm(&mut buf, source, &mut types, &imports)
        .map_err(|e| JsValue::from(format!("{e}")))?;
    String::from_utf8(buf).map_err(|e| JsValue::from(format!("Utf8 error: {e}")))
}

fn default_imports() -> (Vec<FuncType>, Vec<FuncImport>) {
    let types = vec![
        FuncType {
            params: vec![Type::I32],
            results: vec![Type::I32],
        },
        FuncType {
            params: vec![Type::I32, Type::I32, Type::I32],
            results: vec![],
        },
        FuncType {
            params: vec![Type::I32, Type::I32, Type::I32, Type::I32],
            results: vec![],
        },
    ];

    let imports = vec![
        FuncImport {
            module: "console".to_string(),
            name: "log".to_string(),
            ty: 0,
        },
        FuncImport {
            module: "output".to_string(),
            name: "putc".to_string(),
            ty: 0,
        },
        FuncImport {
            module: "canvas".to_string(),
            name: "set_fill_style".to_string(),
            ty: 1,
        },
        FuncImport {
            module: "canvas".to_string(),
            name: "rectangle".to_string(),
            ty: 2,
        },
    ];

    (types, imports)
}
