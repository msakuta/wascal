import { compile, parse_ast, disasm,
    // type_check, run_script, parse_ast, compile, disasm, compile_and_run
 } from "../pkg/index.js";


const consoleElem = document.getElementById("console");

async function runCommon(process) {
    // Clear output
    consoleElem.value = "";
    const canvas = document.getElementById("canvas");
    const canvasRect = canvas.getBoundingClientRect();
    canvas.getContext("2d").clearRect(0, 0, canvasRect.width, canvasRect.height);

    const source = document.getElementById("input").value;
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

async function callFunc(obj, expName) {
    const expFunc = obj.instance.exports[expName];
    console.log("Calling wasm");
    const argElems = functionElems[expName];
    let args = [];
    for (let i = 0; i < expFunc.length; i++) {
        const x = parseFloat(argElems[i].value);
        args.push(x);
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
    }
};

let functionElems = {};

document.getElementById("parseAst").addEventListener("click", () => runCommon(source => {
    const result = parse_ast(source);
    document.getElementById("console").value = result;
}));
document.getElementById("compile").addEventListener("click", () => runCommon(async source => {
    const wasm = compile(source);
    document.getElementById("console").value = wasm;

    const obj = await WebAssembly.instantiate(wasm, opts);

    const functions = document.getElementById("functions");
    while (functions.firstChild) functions.removeChild(functions.firstChild);
    functionElems = [];
    for (let expName in obj.instance.exports) {
        const expFunc = obj.instance.exports[expName];
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
        callButton.addEventListener("click", () => callFunc(obj, expName));
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

document.getElementById("input").value = `
let hello(x, y) = x + y;
`;

const samples = document.getElementById("samples");

[
    "hello.wscl", "fact.wscl", "fibo.wscl", "float.wscl",
    "funcs.wscl", "log.wscl", "loop.wscl",
    "mandel.wscl",
]
    .forEach(fileName => {
    const link = document.createElement("a");
    link.href = "#";
    link.addEventListener("click", () => {
        fetch("scripts/" + fileName)
            .then(file => file.text())
            .then(text => document.getElementById("input").value = text);
    });
    link.innerHTML = fileName;
    samples.appendChild(link);
    samples.append(" ");
})
