export let outputBuf = "";

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
(async () => {
    obj = await WebAssembly.instantiateStreaming(fetch("wascal.wasm"), opts);
    memory = obj.instance.exports.memory;
})();

function addStringToWasm(s) {
    if (!obj) return;
    const view32 = new Int32Array(obj.instance.exports.memory.buffer);
    const top = view32[0];
    const textEncoder = new TextEncoder('utf8');
    const encodedString = textEncoder.encode(s);
    const view8 = new Uint8Array(obj.instance.exports.memory.buffer, top + 4, encodedString.length);
    view8.set(encodedString);
    const newTop = top + Math.floor(encodedString.length + 3 / 4) * 4;
    view32[0] = newTop;
    return [top, encodedString.length];
}

function returnString(s) {
    const view32 = new Int32Array(obj.instance.exports.memory.buffer, s + 4);
    const view8 = new Int8Array(obj.instance.exports.memory.buffer, s + 8, view32[0]);
    const textDecoder = new TextDecoder('utf-8');
    return textDecoder.decode(view8);
}
