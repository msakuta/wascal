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

let obj;
export let memory;
export async function init(wasm, moreOpts = {}){
    for (const key in moreOpts) {
        opts[key] = moreOpts[key];
    }
    if (wasm instanceof Promise) {
        obj = await WebAssembly.instantiateStreaming(wasm, opts);
    }
    else {
        obj = await WebAssembly.instantiate(wasm, opts);
    }
    memory = obj.instance.exports.memory;
}

function addStringToWasm(s) {
    if (!obj) return;
    const view32 = new Int32Array(obj.instance.exports.memory.buffer);
    const top = view32[0];
    const textEncoder = new TextEncoder('utf8');
    const encodedString = textEncoder.encode(s);
    const lengthView = new Uint32Array(obj.instance.exports.memory.buffer, top, 1);
    lengthView[0] = encodedString.length;
    const view8 = new Uint8Array(obj.instance.exports.memory.buffer, top + 4, encodedString.length);
    view8.set(encodedString);
    const newTop = top + Math.floor(encodedString.length + 3 / 4) * 4;
    view32[0] = newTop;
    return top;
}

function returnString(s) {
    const view32 = new Int32Array(obj.instance.exports.memory.buffer, s);
    const view8 = new Int8Array(obj.instance.exports.memory.buffer, s + 4, view32[0]);
    const textDecoder = new TextDecoder('utf-8');
    return textDecoder.decode(view8);
}
