mod compiler;
mod infer;
mod model;
mod parser;
mod wasm_file;

use model::{FuncImport, FuncType, Type};
use wasm_file::compile_wasm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut f = std::io::BufWriter::new(std::fs::File::create("wascal.wasm")?);

    let mut file_name = "scripts/hello.wscl".to_string();
    let mut debug_type_infer = false;
    for arg in std::env::args().skip(1) {
        match &arg as &str {
            "-d" => debug_type_infer = true,
            _ => file_name = arg,
        }
    }

    let source = std::fs::read_to_string(file_name)?;

    let mut types = vec![FuncType {
        params: vec![Type::I32],
        results: vec![Type::I32],
    }];

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
    ];

    compile_wasm(
        &mut f,
        &source,
        &mut types,
        &imports,
        Some(&mut std::io::stdout()),
        Some(&mut std::io::stdout()),
        debug_type_infer,
    )?;

    Ok(())
}
