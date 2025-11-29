use std::{cell::RefCell, sync::Mutex};

use wasmtime::*;
use chibiwasm::{module::ExternalFuncInst, Importer, Runtime, Value};

struct MyState;

fn main() -> Result<()> {
    println!("Compiling module...");

    let use_wasmtime = false;

    if use_wasmtime {
        run_wasmtime()?;
    } else {
        run_chibiwasm()?;
    }

    Ok(())
}

struct Imported {
    name: String,
}

impl Importer for Imported {
    fn invoke(
            &self,
            _store: std::rc::Rc<RefCell<chibiwasm::Store>>,
            func: chibiwasm::module::ExternalFuncInst,
            args: Vec<Value>,
        ) -> Result<Option<Value>> {
            if func.field.as_str() == "putc" {
                if let Value::I32(val) = args[0] {
                    println!("{val}");
                }
            }
            Ok(None)
    }

    fn name(&self) -> &str {
        dbg!("wasi_ephemeral_nn")
    }
}

fn run_chibiwasm() -> anyhow::Result<()> {
    let mut runtime = Runtime::from_file("../wascal.wasm", Some(vec![
        Box::new(Imported {
            name: "output".to_string(),
        })
    ]))?;
    println!("chibiwasm Runtime instantiated!");
    if let Some(output) = runtime.call("main".into(), vec![Value::I32(10), Value::I32(100)])? {
        println!("output: {}", output);
    }
    Ok(())
}

fn run_wasmtime() -> Result<()> {
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
