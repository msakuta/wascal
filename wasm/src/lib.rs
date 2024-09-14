use wasm_bindgen::prelude::*;

use wascal::{default_types, disasm_wasm, typeinf_wasm, FuncImport, FuncType, Type, WasmCompiler};

#[wasm_bindgen]
pub fn parse_ast(source: &str) -> Result<String, JsValue> {
    let (types, imports) = default_imports();
    let mut buf = vec![];
    typeinf_wasm(&mut buf, source, types, imports).map_err(|e| JsValue::from(format!("{e}")))?;
    String::from_utf8(buf).map_err(|e| JsValue::from(format!("Utf8 error: {e}")))
}

#[wasm_bindgen]
pub fn compile(source: &str) -> Result<JsValue, JsValue> {
    let (types, imports) = default_imports();
    let mut wasm_buf = vec![];
    let mut bind_buf = vec![];
    let mut wasm_compiler = WasmCompiler::new(source, types, imports);
    wasm_compiler
        .compile_wasm(&mut wasm_buf, &mut bind_buf)
        .map_err(|e| JsValue::from(format!("{e}")))?;
    let bind_str = String::from_utf8(bind_buf).map_err(|e| JsValue::from(format!("{e}")))?;
    let ret =
        JsValue::from(Box::new([JsValue::from(wasm_buf), JsValue::from(bind_str)]) as Box<[_]>);
    Ok(ret)
}

#[wasm_bindgen]
pub fn disasm(source: &str) -> Result<String, JsValue> {
    let (types, imports) = default_imports();
    let mut buf = vec![];
    disasm_wasm(&mut buf, source, types, imports).map_err(|e| JsValue::from(format!("{e}")))?;
    String::from_utf8(buf).map_err(|e| JsValue::from(format!("Utf8 error: {e}")))
}

fn default_imports() -> (Vec<FuncType>, Vec<FuncImport>) {
    let mut types = default_types();
    let num_default_types = types.len();
    types.extend([
        FuncType {
            params: vec![Type::I32, Type::I32, Type::I32],
            results: vec![],
        },
        FuncType {
            params: vec![Type::I32, Type::I32, Type::I32, Type::I32],
            results: vec![],
        },
    ]);

    let mut imports = wascal::default_imports();
    imports.extend([
        FuncImport {
            module: "canvas".to_string(),
            name: "set_fill_style".to_string(),
            ty: num_default_types,
        },
        FuncImport {
            module: "canvas".to_string(),
            name: "rectangle".to_string(),
            ty: num_default_types + 1,
        },
    ]);

    (types, imports)
}
