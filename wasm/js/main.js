import * as wasm from "../pkg/index_bg.wasm";
import { __wbg_set_wasm } from "../pkg/index_bg.js";
__wbg_set_wasm(wasm);
import { compile, parse_ast, disasm } from "../pkg/index_bg.js";

import { StreamLanguage } from "@codemirror/language"
import { EditorState } from "@codemirror/state"
import { Parser } from "./parser"

import { EditorView, basicSetup } from "codemirror"



const consoleElem = document.getElementById("console");

async function runCommon(process) {
    // Clear output
    consoleElem.value = "";
    const canvas = document.getElementById("canvas");
    const canvasRect = canvas.getBoundingClientRect();
    canvas.getContext("2d").clearRect(0, 0, canvasRect.width, canvasRect.height);

    const source = view.state.doc.toString();
    const start = performance.now();
    try{
        process(source);
    }
    catch(e){
        consoleElem.value = e;
    }
    const end = performance.now();
    document.getElementById("timeMessage").innerHTML = `Execution time: ${(end - start).toFixed(1)} ms (See <a href="#Time">notes</a>)`;
}

async function callFunc(bind, expName) {
    const expFunc = bind[expName];
    console.log("Calling wasm");
    const argElems = functionElems[expName];
    let args = [];
    for (let i = 0; i < expFunc.length; i++) {
        args.push(argElems[i].value);
    }
    let result;
    const start = performance.now();
    outputBuf = "";
    try{
        result = expFunc.apply(this, args);
        outputBuf += `\nResult: ${result}`;
    }
    catch(e){
        outputBuf = e;
    }
    const end = performance.now();
    consoleElem.value = outputBuf;
    document.getElementById("timeMessage").innerHTML = `Execution time: ${(end - start).toFixed(1)} ms (See <a href="#Time">notes</a>)`;
};

let outputBuf = "";
const opts = {
    console: {
        log: console.log,
    },
    output: {
        putc: c => {
            outputBuf += String.fromCharCode(c);
        }
    },
    canvas: {
        set_fill_style: (r, g, b) => {
            const canvas = document.getElementById("canvas");
            const ctx = canvas.getContext('2d');
            const toDigits = x => x < 16 ? `0${x.toString(16)}` : 255 < x ? 'ff' : x.toString(16);
            ctx.fillStyle = `#${toDigits(r)}${toDigits(g)}${toDigits(b)}`;
        },
        rectangle: (x0, y0, x1, y1) => {
            const canvas = document.getElementById("canvas");
            const ctx = canvas.getContext('2d');
            ctx.fillRect(x0, y0, x1, y1);
        },
    }
};

let functionElems = {};

document.getElementById("parseAst").addEventListener("click", () => runCommon(source => {
    const result = parse_ast(source);
    document.getElementById("console").value = result;
}));
document.getElementById("compile").addEventListener("click", () => runCommon(async source => {
    let bind;
    try {
        const [wasm, bindStr] = compile(source);
        bind = eval(bindStr);
        bind.init(wasm, opts);
        consoleElem.value = `Compiled WebAssembly module in ${wasm.length} bytes.`;
    }
    catch(e) {
        consoleElem.value = `Compile failed: ${e}`;
    }

    const functions = document.getElementById("functions");
    while (functions.firstChild) functions.removeChild(functions.firstChild);
    functionElems = [];
    console.log(`bind: ${Object.getOwnPropertyNames(bind)}`);
    for (const expName in bind) {
        // Skip useless functions from UI
        if (0 <= ["init", "malloc", "set", "strcat"].indexOf(expName)) continue;
        const expFunc = bind[expName];
        if (typeof expFunc !== "function") continue;
        const funcElem = document.createElement("div");
        const fNameElem = document.createTextNode(expName + "(");
        funcElem.appendChild(fNameElem);

        const argElems = [];

        for (let i = 0; i < expFunc.length; i++) {
            const label = document.createElement("label");
            const input = document.createElement("input");
            input.setAttribute("type", "text");
            input.value = 1;
            label.appendChild(input);
            funcElem.appendChild(label);
            if (i < expFunc.length - 1) {
                funcElem.appendChild(document.createTextNode(", "));
            }
            argElems.push(input);
        }
        funcElem.appendChild(document.createTextNode(")"));

        const callButton = document.createElement("button");
        callButton.innerHTML = "Call";
        callButton.addEventListener("click", () => callFunc(bind, expName));
        funcElem.appendChild(callButton);

        functions.appendChild(funcElem);
        functionElems[expName] = argElems;
    }
}));
document.getElementById("disasm").addEventListener("click", () => runCommon(source => {
    const result = disasm(source);
    document.getElementById("console").value = result;
}));
document.getElementById("clearCanvas").addEventListener("click", () => {
    const canvas = document.getElementById("canvas");
    const canvasRect = canvas.getBoundingClientRect();
    canvas.getContext("2d").clearRect(0, 0, canvasRect.width, canvasRect.height);
});

const samples = document.getElementById("samples");

[
    "hello.wscl", "fact.wscl", "fibo.wscl", "float.wscl",
    "funcs.wscl", "log.wscl", "loop.wscl",
    "string.wscl", "strrepeat.wscl",
    "struct.wscl", "struct_infer.wscl", "struct_nested.wscl",
    "vector.wscl", "field_assign.wscl",
    "mandel.wscl",
    "canvas.wscl", "mandel_canvas.wscl",
]
    .forEach(fileName => {
    const link = document.createElement("a");
    link.href = "#";
    link.addEventListener("click", async () => {
        const file = await fetch("scripts/" + fileName);
        const text = await file.text();
        let size = view.state.doc.length;
        const trans = view.state.update(
            {changes: {from: 0, to: size}, sequential: true},
            {changes: {from: 0, insert: text}, sequential: true});
        view.dispatch(trans);
    });
    link.innerHTML = fileName;
    samples.appendChild(link);
    samples.append(" ");
})

let initState = EditorState.create({
    extensions: [basicSetup, StreamLanguage.define(Parser)],
    doc: "pub let hello(x: i32, y: i32) = x + y;",
});

let view = new EditorView({
    state: initState,
    parent: document.getElementById("highlighting"),
})
