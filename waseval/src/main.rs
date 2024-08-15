use std::{cell::RefCell, sync::Mutex};

use wasmtime::*;

struct MyState;

fn main() -> Result<()> {
    println!("Compiling module...");
    let engine = Engine::default();
    let module = Module::from_file(&engine, "../wascal.wasm")?;

    println!("Initializing...");
    let mut store = Store::new(
        &engine,
        MyState,
    );

    println!("Creating callback...");
    let log_func = Func::wrap(&mut store, |arg: i32| -> i32 {
        println!("console.log: {}", arg);
        0
    });
    // let output = Mutex::new(String::new());
    let putc_func = Func::wrap(&mut store, |arg: i32| -> i32 {
        // output.lock().unwrap().push(arg as u8 as char);
        print!("{}", arg as u8 as char);
        0
    });

    println!("Instantiating module...");
    let imports = [log_func.into(), putc_func.into()];
    let instance = Instance::new(&mut store, &module, &imports)?;

    println!("Extracting export...");
    let run = instance.get_typed_func::<(), i32>(&mut store, "hello")?;

    println!("Calling export...");
    run.call(&mut store, ())?;

    Ok(())
}
