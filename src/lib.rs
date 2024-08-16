mod compiler;
mod model;
mod parser;
mod wasm_file;

pub use crate::{
    parser::parse,
    wasm_file::{compile_wasm, disasm_wasm},
};
