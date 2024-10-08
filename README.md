# Wascal

This is a language parser/compiler into WebAssembly written in pure Rust _without any dependencies_.

Try it now on your browser! https://msakuta.github.io/wascal

The bytecode is produced directly from the crate without using any library.
People usually use LLVM or Cranelift to generate code, but WebAssembly spec is [fully open and relatively simple](https://www.w3.org/TR/wasm-core-1/), so nothing stops you from writing everything from scratch.
It could be suboptimal, but I have full control on the code generation.

## Performance

My main interest is the performance among other implementations of runtime.

Here is my simple Mandelbrot set ASCII art rendering time comparison.
Surprisingly, Wasm is quite fast, even faster than native compiled code via LLVM IR (implemented in [inkwell-ruscal](https://github.com/msakuta/inkwell-ruscal)).
There are other implementations using AST interpreter and my own bytecode interpreter (implemented in [ruscal](https://github.com/msakuta/ruscal)), but they are very slow.
I also put Go, Ruby, Java (OpenJDK), Python and JavaScript (Node.js) as a comparison with popular languages.
I also put native C and Rust compiled code, but of course they are the fastest.

![performance](images/mandel-time.png)

A special note is needed for `Go run` and `Go build`.
Both are execution time for Golang, but `Go run` is the time of the whole `go run` command, which includes compilation.
`Go build` is a time of executing the executable produced by `go build` command, excluding the compilation time.
Since other scrpting languages like Ruby or Python include compile time of the source text, I think it's more fair to compare with the compilation time, but we are also interested in execution time with ahead-of-time compiled executable, so I put them both.

Below is the actual output.
You can see the script in [mandel_highres.wscl](scripts/mandel_highres.wscl).

![mandel](images/mandel-ascii.png)

## How to run

1. Install Rust
2. Save the file like below as `scripts/hello.wasm`

```
let hello(x, y) = x + y
```

3. Compile the code

```
cargo run scripts/hello.wscl
```

It will produce a wasm binary file `wascal.wasm`

4. Run it by

```
npx serve
```

Open a browser and browse http://localhost:3000

![browser screenshot](images/screenshot.png)

You can choose an exported function from Wasm module and call it with given arguments.
The result will be printed on `Output` and also output text will be printed in the readonly text area below.

## How to run with wasmtime

In order to measure native performance, we would like to run on a cli, not on the browser.
We can use wasmtime to do that in Rust code.

Go to waseval child crate:

```
cd waseval
```

and run it

```
cargo r --release
```

Be sure to use `--release` flag to measure optimized performance.

You may even separate build and run steps to measure only the running time.

```
cargo b --release
time target/release/waseval
```

## How to run the compiler on a browser to compile and run Wasm

You can also build a wasm compiler in wasm and run on the browser,
which can compile a source code into wasm and run on the browser
(yes it's disorienting but that's what it does).

    cd wasm
    npm run build

To launch the application, you can use `npx`

    cd dist
    npx serve

and browse http://localhost:5000.
