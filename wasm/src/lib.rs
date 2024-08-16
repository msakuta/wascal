use wasm_bindgen::prelude::*;

use wascal::{compile_wasm, disasm_wasm, parse};

#[wasm_bindgen]
pub fn parse_ast(source: &str) -> Result<String, JsValue> {
    let ast = parse(source).map_err(JsValue::from)?;
    Ok(format!("{ast:#?}"))
}

#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Vec<u8>, JsValue> {
    let mut buf = vec![];
    compile_wasm(&mut buf, source).map_err(|e| JsValue::from(format!("{e}")))?;
    Ok(buf)
}

#[wasm_bindgen]
pub fn disasm(source: &str) -> Result<String, JsValue> {
    let mut buf = vec![];
    disasm_wasm(&mut buf, source).map_err(|e| JsValue::from(format!("{e}")))?;
    String::from_utf8(buf).map_err(|e| JsValue::from(format!("Utf8 error: {e}")))
}
