import {
    WASI,
    File,
    OpenFile,
    ConsoleStdout,
} from "https://esm.sh/@bjorn3/browser_wasi_shim@0.3.0";

const $ = (id) => document.getElementById(id);

const THEME_KEY = "mpf-demo-theme";

const applyTheme = (theme) => {
    const toggle = $("theme-toggle");
    if (theme === "light") {
        document.documentElement.setAttribute("data-theme", "light");
        if (toggle) toggle.textContent = "Dark";
    } else {
        document.documentElement.removeAttribute("data-theme");
        if (toggle) toggle.textContent = "Light";
    }
};

const initialTheme = (() => {
    const stored = localStorage.getItem(THEME_KEY);
    if (stored === "light" || stored === "dark") return stored;
    return window.matchMedia("(prefers-color-scheme: light)").matches
        ? "light"
        : "dark";
})();

applyTheme(initialTheme);

$("theme-toggle").addEventListener("click", () => {
    const current =
        document.documentElement.getAttribute("data-theme") === "light"
            ? "light"
            : "dark";
    const next = current === "light" ? "dark" : "light";
    localStorage.setItem(THEME_KEY, next);
    applyTheme(next);
});

const bytesToHex = (bs) =>
    [...bs].map((b) => b.toString(16).padStart(2, "0")).join("");

const hexToBytes = (s) => {
    const clean = s.replace(/\s+/g, "");
    const out = new Uint8Array(clean.length / 2);
    for (let i = 0; i < out.length; i++) {
        out[i] = parseInt(clean.substr(i * 2, 2), 16);
    }
    return out;
};

const utf8 = new TextEncoder();
const utf8Decoder = new TextDecoder();

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

const u32be = (n) => {
    const out = new Uint8Array(4);
    out[0] = (n >>> 24) & 0xff;
    out[1] = (n >>> 16) & 0xff;
    out[2] = (n >>> 8) & 0xff;
    out[3] = n & 0xff;
    return out;
};

const lenPrefixed = (bs) => concatBytes([u32be(bs.length), bs]);

class Reader {
    constructor(bs) {
        this.bs = bs;
        this.off = 0;
    }
    u32() {
        const [a, b, c, d] = [
            this.bs[this.off],
            this.bs[this.off + 1],
            this.bs[this.off + 2],
            this.bs[this.off + 3],
        ];
        this.off += 4;
        return ((a << 24) | (b << 16) | (c << 8) | d) >>> 0;
    }
    take(n) {
        const out = this.bs.subarray(this.off, this.off + n);
        this.off += n;
        return out;
    }
    lenBytes() {
        return this.take(this.u32());
    }
}

const DB_NAME = "mpf-browser-demo";
const STORE = "state";
const STATE_KEY = "mts-state";
const KVLIST_KEY = "mts-kvlist";
const UNDO_KEY = "mts-undo-stack";
const REDO_KEY = "mts-redo-stack";
const MAX_HISTORY = 50;

const openDB = () =>
    new Promise((resolve, reject) => {
        const req = indexedDB.open(DB_NAME, 1);
        req.onupgradeneeded = () => req.result.createObjectStore(STORE);
        req.onsuccess = () => resolve(req.result);
        req.onerror = () => reject(req.error);
    });

const idbGet = async (key) => {
    const db = await openDB();
    return new Promise((resolve, reject) => {
        const tx = db.transaction(STORE, "readonly");
        const req = tx.objectStore(STORE).get(key);
        req.onsuccess = () => resolve(req.result ?? null);
        req.onerror = () => reject(req.error);
    });
};

const idbPut = async (key, value) => {
    const db = await openDB();
    return new Promise((resolve, reject) => {
        const tx = db.transaction(STORE, "readwrite");
        tx.objectStore(STORE).put(value, key);
        tx.oncomplete = () => resolve();
        tx.onerror = () => reject(tx.error);
    });
};

const idbDelete = async (key) => {
    const db = await openDB();
    return new Promise((resolve, reject) => {
        const tx = db.transaction(STORE, "readwrite");
        tx.objectStore(STORE).delete(key);
        tx.oncomplete = () => resolve();
        tx.onerror = () => reject(tx.error);
    });
};

const loadWasm = async (url, sizeEl) => {
    const resp = await fetch(url);
    const bytes = new Uint8Array(await resp.arrayBuffer());
    if (sizeEl) sizeEl.textContent = bytes.length.toLocaleString();
    return WebAssembly.compile(bytes);
};

const writeMod = await loadWasm(
    new URL("./mpf-write.wasm", import.meta.url),
    $("size-write"),
);
const verifyMod = await loadWasm(
    new URL("./mpf-verify.wasm", import.meta.url),
    $("size-verify"),
);

const runWasm = async (module, stdinBytes, logEl) => {
    const logLines = [];
    const stdinFd = new OpenFile(new File(stdinBytes));
    const stdoutFile = new File([]);
    const stdoutFd = new OpenFile(stdoutFile);
    const stderrFd = ConsoleStdout.lineBuffered((line) =>
        logLines.push(line),
    );
    const wasi = new WASI([], [], [stdinFd, stdoutFd, stderrFd]);
    const instance = await WebAssembly.instantiate(module, {
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
            logLines.push(`[throw] ${e.message || e}`);
            exitCode = -1;
        }
    }
    if (logEl) logEl.textContent = logLines.join("\n") || "(no output)";
    return { exitCode, stdout: new Uint8Array(stdoutFile.data) };
};

const OP_INSERT = 0;
const OP_DELETE = 1;

const PT_INCLUSION = 0;
const PT_EXCLUSION = 1;
const PT_NONE = 0xff;

const runWrite = async (priorState, ops, queryKey) => {
    const parts = [lenPrefixed(priorState || new Uint8Array(0))];
    parts.push(u32be(ops.length));
    for (const op of ops) {
        if (op.type === "insert") {
            parts.push(new Uint8Array([OP_INSERT]));
            parts.push(lenPrefixed(op.key));
            parts.push(lenPrefixed(op.value));
        } else if (op.type === "delete") {
            parts.push(new Uint8Array([OP_DELETE]));
            parts.push(lenPrefixed(op.key));
        } else {
            throw new Error(`unknown op type: ${op.type}`);
        }
    }
    parts.push(lenPrefixed(queryKey || new Uint8Array(0)));
    const stdin = concatBytes(parts);

    const { stdout } = await runWasm(writeMod, stdin, $("log-write"));
    const r = new Reader(stdout);
    const newState = r.lenBytes().slice();
    const root = r.take(32).slice();
    const value = r.lenBytes().slice();
    const ptype = r.take(1)[0];
    const proof = r.lenBytes().slice();
    return { newState, root, value, ptype, proof };
};

const runVerify = async (root, key, value, proof, ptype) => {
    const stdin = concatBytes([
        new Uint8Array([ptype]),
        root,
        lenPrefixed(key),
        lenPrefixed(value),
        lenPrefixed(proof),
    ]);
    const { exitCode } = await runWasm(verifyMod, stdin, $("log-verify"));
    return exitCode === 0;
};

let kvLog = [];
let priorState = new Uint8Array(0);
let undoStack = [];
let redoStack = [];

const kvBody = $("kv-body");

const snapshot = () => ({
    priorState,
    kvLog: kvLog.slice(),
});

const persistHistory = async () => {
    await idbPut(UNDO_KEY, undoStack);
    await idbPut(REDO_KEY, redoStack);
};

const recordMutation = () => {
    undoStack.push(snapshot());
    if (undoStack.length > MAX_HISTORY) undoStack.shift();
    redoStack = [];
    updateHistoryButtons();
};

const updateHistoryButtons = () => {
    $("undo").disabled = undoStack.length === 0;
    $("redo").disabled = redoStack.length === 0;
};

const latestMap = () => {
    const latest = new Map();
    for (const entry of kvLog) {
        if (entry.type === "insert") latest.set(entry.key, entry.value);
        else if (entry.type === "delete") latest.delete(entry.key);
    }
    return latest;
};

const refreshTable = () => {
    kvBody.textContent = "";
    const latest = latestMap();
    for (const [k, v] of latest) {
        const tr = document.createElement("tr");
        const kd = document.createElement("td");
        const vd = document.createElement("td");
        const ad = document.createElement("td");
        kd.textContent = k;
        vd.textContent = v;
        const del = document.createElement("button");
        del.type = "button";
        del.className = "danger";
        del.textContent = "Delete";
        del.addEventListener("click", async () => {
            try {
                await deleteKey(k);
            } catch (e) {
                $("log-write").textContent = `error: ${e.message || e}`;
            }
        });
        ad.appendChild(del);
        tr.appendChild(kd);
        tr.appendChild(vd);
        tr.appendChild(ad);
        kvBody.appendChild(tr);
    }
    $("state-summary").textContent =
        `${latest.size} distinct keys, ${priorState.length}-byte state`;
};

const updateRoot = (root) => {
    $("root").value = bytesToHex(root);
};

const insertPair = async (k, v) => {
    const kBytes = utf8.encode(k);
    const vBytes = utf8.encode(v);
    recordMutation();
    const { newState, root } = await runWrite(
        priorState,
        [{ type: "insert", key: kBytes, value: vBytes }],
        kBytes,
    );
    priorState = newState;
    kvLog.push({ type: "insert", key: k, value: v });
    await idbPut(STATE_KEY, priorState);
    await idbPut(KVLIST_KEY, kvLog);
    await persistHistory();
    updateRoot(root);
    refreshTable();
};

const deleteKey = async (k) => {
    const kBytes = utf8.encode(k);
    recordMutation();
    const { newState, root } = await runWrite(
        priorState,
        [{ type: "delete", key: kBytes }],
        kBytes,
    );
    priorState = newState;
    kvLog.push({ type: "delete", key: k });
    await idbPut(STATE_KEY, priorState);
    await idbPut(KVLIST_KEY, kvLog);
    await persistHistory();
    updateRoot(root);
    refreshTable();
};

const restoreSnapshot = async (snap) => {
    priorState = snap.priorState;
    kvLog = snap.kvLog.slice();
    await idbPut(STATE_KEY, priorState);
    await idbPut(KVLIST_KEY, kvLog);
    if (priorState.length > 0) {
        const { root } = await runWrite(
            priorState,
            [],
            new Uint8Array(0),
        );
        updateRoot(root);
    } else {
        $("root").value = "";
    }
    $("proof-value").value = "";
    $("proof-bytes").value = "";
    $("proof-kind").textContent = "";
    $("proof-kind").className = "muted";
    $("proof-type").value = String(PT_NONE);
    $("verify-result").textContent = "idle";
    $("verify-result").className = "muted";
    refreshTable();
};

const doUndo = async () => {
    if (undoStack.length === 0) return;
    redoStack.push(snapshot());
    if (redoStack.length > MAX_HISTORY) redoStack.shift();
    const snap = undoStack.pop();
    await restoreSnapshot(snap);
    await persistHistory();
    updateHistoryButtons();
};

const doRedo = async () => {
    if (redoStack.length === 0) return;
    undoStack.push(snapshot());
    if (undoStack.length > MAX_HISTORY) undoStack.shift();
    const snap = redoStack.pop();
    await restoreSnapshot(snap);
    await persistHistory();
    updateHistoryButtons();
};

$("ins-add").addEventListener("click", async () => {
    const k = $("ins-key").value;
    const v = $("ins-value").value;
    if (!k) return;
    try {
        await insertPair(k, v);
        $("ins-key").value = "";
        $("ins-value").value = "";
    } catch (e) {
        $("log-write").textContent = `error: ${e.message || e}`;
    }
});

$("ins-clear").addEventListener("click", async () => {
    recordMutation();
    priorState = new Uint8Array(0);
    kvLog = [];
    await idbDelete(STATE_KEY);
    await idbDelete(KVLIST_KEY);
    await persistHistory();
    $("root").value = "";
    $("proof-key").value = "";
    $("proof-value").value = "";
    $("proof-bytes").value = "";
    $("proof-kind").textContent = "";
    $("proof-kind").className = "muted";
    $("proof-type").value = String(PT_NONE);
    $("verify-result").textContent = "idle";
    $("verify-result").className = "muted";
    $("log-write").textContent = "";
    $("log-verify").textContent = "";
    refreshTable();
});

$("undo").addEventListener("click", doUndo);
$("redo").addEventListener("click", doRedo);

window.addEventListener("keydown", (e) => {
    if (!(e.metaKey || e.ctrlKey)) return;
    const tag = document.activeElement && document.activeElement.tagName;
    if (tag === "INPUT" || tag === "TEXTAREA") return;
    if (e.key === "z" && !e.shiftKey) {
        e.preventDefault();
        doUndo();
    } else if ((e.key === "z" && e.shiftKey) || e.key === "y") {
        e.preventDefault();
        doRedo();
    }
});

const setProofType = (ptype) => {
    $("proof-type").value = String(ptype);
};
const getProofType = () => Number($("proof-type").value);

$("proof-gen").addEventListener("click", async () => {
    const k = $("proof-key").value;
    if (!k) return;
    try {
        const { root, value, ptype, proof } = await runWrite(
            priorState,
            [],
            utf8.encode(k),
        );
        updateRoot(root);
        setProofType(ptype);
        if (ptype === PT_INCLUSION) {
            $("proof-kind").textContent = "inclusion";
            $("proof-kind").className = "ok";
            $("proof-value").value = utf8Decoder.decode(value);
            $("proof-bytes").value = bytesToHex(proof);
        } else if (ptype === PT_EXCLUSION) {
            $("proof-kind").textContent =
                "exclusion (key is not in the forest)";
            $("proof-kind").className = "muted";
            $("proof-value").value = "";
            $("proof-bytes").value = bytesToHex(proof);
        } else {
            $("proof-kind").textContent = "no proof (forest is empty)";
            $("proof-kind").className = "muted";
            $("proof-value").value = "";
            $("proof-bytes").value = "";
        }
    } catch (e) {
        $("log-write").textContent = `error: ${e.message || e}`;
    }
});

const doVerify = async (tamper) => {
    const resEl = $("verify-result");
    resEl.textContent = "running...";
    resEl.className = "muted";
    try {
        const root = hexToBytes($("root").value);
        const proof = hexToBytes($("proof-bytes").value);
        const key = utf8.encode($("proof-key").value);
        const ptype = getProofType();
        if (
            root.length !== 32 ||
            key.length === 0 ||
            proof.length === 0 ||
            (ptype !== PT_INCLUSION && ptype !== PT_EXCLUSION)
        ) {
            resEl.textContent = "need a root + key + proof first";
            resEl.className = "bad";
            return;
        }
        const value =
            ptype === PT_INCLUSION
                ? utf8.encode($("proof-value").value)
                : new Uint8Array(0);
        let testProof = proof;
        if (tamper) {
            testProof = new Uint8Array(proof);
            testProof[testProof.length - 1] ^= 0xff;
        }
        const ok = await runVerify(root, key, value, testProof, ptype);
        const kind = ptype === PT_INCLUSION ? "inclusion" : "exclusion";
        if (ok) {
            resEl.textContent = tamper
                ? `unexpected: tampered ${kind} proof accepted`
                : `valid ${kind} proof (exit 0)`;
            resEl.className = tamper ? "bad" : "ok";
        } else {
            resEl.textContent = tamper
                ? `rejected (exit 1) — tamper detected on ${kind} proof`
                : `invalid ${kind} proof (exit 1)`;
            resEl.className = tamper ? "ok" : "bad";
        }
    } catch (e) {
        resEl.textContent = `error: ${e.message || e}`;
        resEl.className = "bad";
    }
};

$("verify").addEventListener("click", () => doVerify(false));
$("verify-tamper").addEventListener("click", () => doVerify(true));

(async () => {
    const savedState = await idbGet(STATE_KEY);
    const savedLog = await idbGet(KVLIST_KEY);
    const savedUndo = await idbGet(UNDO_KEY);
    const savedRedo = await idbGet(REDO_KEY);
    if (savedState) priorState = savedState;
    if (Array.isArray(savedLog)) {
        kvLog = savedLog.map((entry) => {
            if (Array.isArray(entry)) {
                return { type: "insert", key: entry[0], value: entry[1] };
            }
            return entry;
        });
    }
    if (Array.isArray(savedUndo)) undoStack = savedUndo;
    if (Array.isArray(savedRedo)) redoStack = savedRedo;
    if (priorState.length > 0) {
        const { root } = await runWrite(priorState, [], new Uint8Array(0));
        updateRoot(root);
    }
    updateHistoryButtons();
    refreshTable();
})();
