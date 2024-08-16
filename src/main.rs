mod compiler;
mod model;
mod parser;
mod wasm_file;

use wasm_file::compile_wasm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut f = std::io::BufWriter::new(std::fs::File::create("wascal.wasm")?);

    let arg = std::env::args()
        .nth(1)
        .unwrap_or("scripts/hello.wscl".to_string());

    let source = std::fs::read_to_string(arg)?;

    compile_wasm(&mut f, &source)?;

    Ok(())
}
