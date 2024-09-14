mod compiler;
mod const_table;
mod infer;
mod leb128;
mod model;
mod parser;
mod wasm_file;

use model::{FuncImport, FuncType, Type};
use wasm_file::{default_imports, default_types, WasmCompiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut f = std::io::BufWriter::new(std::fs::File::create("wascal.wasm")?);

    let mut file_name = "scripts/hello.wscl".to_string();
    let mut bind_f = std::io::BufWriter::new(std::fs::File::create("wascal.js")?);
    let mut debug_type_infer = false;
    let mut debug_count_stack = false;
    let mut enable_disasm = false;
    let mut bind_module = false;
    for arg in std::env::args().skip(1) {
        match &arg as &str {
            "-d" => debug_type_infer = true,
            "-C" => debug_count_stack = true,
            "-D" => enable_disasm = true,
            "-m" => bind_module = true,
            _ => file_name = arg,
        }
    }

    let source = std::fs::read_to_string(file_name)?;

    let types = default_types();
    let imports = default_imports();

    let mut wasm_compiler = WasmCompiler::new(&source, types, imports)
        .bind_module(bind_module)
        .debug_count_stack(debug_count_stack);
    let mut stdout = std::io::stdout();
    if enable_disasm {
        wasm_compiler = wasm_compiler.disasm_f(&mut stdout);
    }
    let mut stdout = std::io::stdout();
    if debug_type_infer {
        wasm_compiler = wasm_compiler.typeinf_f(&mut stdout).debug_type_infer(true);
    }

    wasm_compiler.compile_wasm(&mut f, &mut bind_f)?;

    Ok(())
}
