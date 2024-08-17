mod compiler;
mod model;
mod parser;
mod wasm_file;

pub use crate::{
    model::{FuncImport, FuncType, Type},
    parser::parse,
    wasm_file::{compile_wasm, disasm_wasm},
};
