mod compiler;
mod const_table;
mod infer;
mod leb128;
mod model;
mod parser;
mod wasm_file;

pub use crate::{
    model::{FuncImport, FuncType, Type},
    parser::{format_expr, format_stmt, parse},
    wasm_file::{default_imports, default_types, disasm_wasm, typeinf_wasm, WasmCompiler},
};
