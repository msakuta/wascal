use wasm_bindgen::prelude::*;

use wascal::compile_wasm;

#[wasm_bindgen]
pub fn compile(source: &str) -> Result<Vec<u8>, JsValue> {
    let mut buf = vec![];
    compile_wasm(&mut buf, source).map_err(|e| JsValue::from(format!("{e}")))?;
    Ok(buf)
}
