import {
    WASI,
    File,
    OpenFile,
    ConsoleStdout,
} from "https://esm.sh/@bjorn3/browser_wasi_shim@0.3.0";

const $ = (id) => document.getElementById(id);

const hexToBytes = (s) => {
    const clean = s.replace(/\s+/g, "");
    if (clean.length % 2 !== 0) {
        throw new Error("hex string must have even length");
    }
    const out = new Uint8Array(clean.length / 2);
    for (let i = 0; i < out.length; i++) {
        out[i] = parseInt(clean.substr(i * 2, 2), 16);
    }
    return out;
};

const concatBytes = (parts) => {
    const total = parts.reduce((n, p) => n + p.length, 0);
    const out = new Uint8Array(total);
    let off = 0;
    for (const p of parts) {
        out.set(p, off);
        off += p.length;
    }
    return out;
};

const wasmUrl = new URL("./csmt-verify.wasm", import.meta.url);
const fixturesUrl = new URL("./fixtures.json", import.meta.url);

const [wasmResp, fixtures] = await Promise.all([
    fetch(wasmUrl),
    fetch(fixturesUrl).then((r) => r.json()),
]);
const wasmBytes = new Uint8Array(await wasmResp.arrayBuffer());
$("wasm-size").textContent = wasmBytes.length.toLocaleString();
const wasmModule = await WebAssembly.compile(wasmBytes);

const options = [];
for (let i = 0; i < fixtures.proofs.length; i++) {
    const p = fixtures.proofs[i];
    options.push({
        label: `inclusion: key=${p.key} value=${p.value}`,
        opcode: 0,
        root: fixtures.rootHash,
        proof: p.cbor,
    });
}
for (let i = 0; i < fixtures.exclusionProofs.length; i++) {
    const p = fixtures.exclusionProofs[i];
    options.push({
        label: `exclusion: key=${p.targetKey}`,
        opcode: 1,
        root: fixtures.rootHash,
        proof: p.cbor,
    });
}

const fixtureSel = $("fixture");
for (let i = 0; i < options.length; i++) {
    const opt = document.createElement("option");
    opt.value = String(i);
    opt.textContent = options[i].label;
    fixtureSel.appendChild(opt);
}

const applyFixture = (idx) => {
    const o = options[idx];
    $("opcode").value = String(o.opcode);
    $("root").value = o.root;
    $("proof").value = o.proof;
    $("result").textContent = "idle";
    $("result").className = "muted";
    $("log").textContent = "";
};
applyFixture(0);
fixtureSel.addEventListener("change", (e) =>
    applyFixture(Number(e.target.value)),
);

$("tamper-root").addEventListener("click", () => {
    const bytes = hexToBytes($("root").value);
    bytes[0] ^= 0xff;
    $("root").value = [...bytes]
        .map((b) => b.toString(16).padStart(2, "0"))
        .join("");
});

$("tamper-proof").addEventListener("click", () => {
    const bytes = hexToBytes($("proof").value);
    if (bytes.length > 0) bytes[bytes.length - 1] ^= 0xff;
    $("proof").value = [...bytes]
        .map((b) => b.toString(16).padStart(2, "0"))
        .join("");
});

const runVerifier = async (opcode, root, proof) => {
    if (root.length !== 32) {
        throw new Error(
            `root must be 32 bytes, got ${root.length}`,
        );
    }
    const stdinBytes = concatBytes([new Uint8Array([opcode]), root, proof]);

    const logLines = [];
    const stdoutFd = ConsoleStdout.lineBuffered((line) =>
        logLines.push(`[stdout] ${line}`),
    );
    const stderrFd = ConsoleStdout.lineBuffered((line) =>
        logLines.push(`[stderr] ${line}`),
    );
    const stdinFd = new OpenFile(new File(stdinBytes));

    const wasi = new WASI([], [], [stdinFd, stdoutFd, stderrFd]);
    const instance = await WebAssembly.instantiate(wasmModule, {
        wasi_snapshot_preview1: wasi.wasiImport,
    });
    let exitCode = 0;
    try {
        const rc = wasi.start(instance);
        if (typeof rc === "number") exitCode = rc;
    } catch (e) {
        if (e && typeof e.code === "number") {
            exitCode = e.code;
        } else {
            throw e;
        }
    }
    return { exitCode, log: logLines.join("\n") };
};

$("verify").addEventListener("click", async () => {
    const resultEl = $("result");
    const logEl = $("log");
    resultEl.textContent = "running...";
    resultEl.className = "muted";
    logEl.textContent = "";
    try {
        const opcode = Number($("opcode").value);
        const root = hexToBytes($("root").value);
        const proof = hexToBytes($("proof").value);
        const { exitCode, log } = await runVerifier(opcode, root, proof);
        logEl.textContent = log || "(no output)";
        if (exitCode === 0) {
            resultEl.textContent = "valid (exit 0)";
            resultEl.className = "ok";
        } else {
            resultEl.textContent = `invalid (exit ${exitCode})`;
            resultEl.className = "bad";
        }
    } catch (e) {
        resultEl.textContent = `error: ${e.message}`;
        resultEl.className = "bad";
    }
});
