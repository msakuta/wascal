mod compiler;
mod infer;
mod model;
mod parser;
mod wasm_file;

pub use crate::{
    model::{FuncImport, FuncType, Type},
    parser::{format_expr, format_stmt, parse},
    wasm_file::{compile_wasm, disasm_wasm, typeinf_wasm},
};
