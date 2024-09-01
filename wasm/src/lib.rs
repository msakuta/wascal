use wasm_bindgen::prelude::*;

use wascal::{compile_wasm, disasm_wasm, typeinf_wasm, FuncImport, FuncType, Type};

#[wasm_bindgen]
pub fn parse_ast(source: &str) -> Result<String, JsValue> {
    let (mut types, imports) = default_imports();
    let mut buf = vec![];
    typeinf_wasm(&mut buf, source, &mut types, &imports)
        .map_err(|e| JsValue::from(format!("{e}")))?;
    String::from_utf8(buf).map_err(|e| JsValue::from(format!("Utf8 error: {e}")))
}

#[wasm_bindgen]
pub fn compile(source: &str) -> Result<JsValue, JsValue> {
    let (mut types, imports) = default_imports();
    let mut wasm_buf = vec![];
    let mut bind_buf = vec![];
    compile_wasm(
        &mut wasm_buf,
        &mut bind_buf,
        false,
        source,
        &mut types,
        &imports,
        None,
        None,
        false,
    )
    .map_err(|e| JsValue::from(format!("{e}")))?;
    let bind_str = String::from_utf8(bind_buf).map_err(|e| JsValue::from(format!("{e}")))?;
    let ret =
        JsValue::from(Box::new([JsValue::from(wasm_buf), JsValue::from(bind_str)]) as Box<[_]>);
    Ok(ret)
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
