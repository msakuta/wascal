# Wascal

This is a language parser/compiler into WebAssembly.

# How to run

Install Rust

```
cargo run "1+2*3"
```

It will produce a wasm binary file `wascal.wasm`

Run it by

```
npx serve
```

Open a browser and browse http://localhost:3000

It should print `6`
