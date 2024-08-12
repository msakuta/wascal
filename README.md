# Wascal

This is a language parser/compiler into WebAssembly.

# How to run

1. Install Rust
2. Save the file like below as `scripts/hello.wasm`

```
let hello(x y) = x + y
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

You can give 2 arguments
