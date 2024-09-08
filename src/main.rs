mod compiler;
mod const_table;
mod infer;
mod model;
mod parser;
mod wasm_file;

use model::{FuncImport, FuncType, Type};
use wasm_file::{compile_wasm, default_imports, default_types};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut f = std::io::BufWriter::new(std::fs::File::create("wascal.wasm")?);

    let mut file_name = "scripts/hello.wscl".to_string();
    let mut bind_f = std::io::BufWriter::new(std::fs::File::create("wascal.js")?);
    let mut debug_type_infer = false;
    let mut enable_disasm = false;
    let mut bind_module = false;
    for arg in std::env::args().skip(1) {
        match &arg as &str {
            "-d" => debug_type_infer = true,
            "-D" => enable_disasm = true,
            "-m" => bind_module = true,
            _ => file_name = arg,
        }
    }

    let source = std::fs::read_to_string(file_name)?;

    let mut types = default_types();
    let imports = default_imports();

    let mut disasm_f = if enable_disasm {
        Some(std::io::stdout())
    } else {
        None
    };
    compile_wasm(
        &mut f,
        &mut bind_f,
        bind_module,
        &source,
        &mut types,
        &imports,
        disasm_f.as_mut().map(|p| p as &mut dyn std::io::Write),
        Some(&mut std::io::stdout()),
        debug_type_infer,
    )?;

    Ok(())
}
