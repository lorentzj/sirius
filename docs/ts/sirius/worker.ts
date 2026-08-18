import init, { compile } from "./wasm/sirius";
import type { EditRequest, Diagnostic } from "./lints";

let initWasm = false;

onmessage = async (e: MessageEvent<EditRequest>) => {
    if(!initWasm) {
        await init();
        initWasm = true;
    }

    const output = compile(e.data.code);
    const lineOffsets = [0, ...[...e.data.code.matchAll(/\n/g)].map(match => match.index + 1)];
    const diagnostics: Diagnostic[] = [];

    for(const error of output.errors) {
        diagnostics.push({
            from: lineOffsets[error.start_line] + error.start_column,
            to: lineOffsets[error.end_line] + error.end_column,
            severity: "error",
            message: `${error.type}: ${error.message}`
        });
    }

    for(const type_token of output.type_tokens) {
        diagnostics.push({
            from: lineOffsets[type_token.line] + type_token.start,
            to: lineOffsets[type_token.line] + type_token.end,
            severity: "info",
            message: "Type",
            markClass: "type"
        });
    }
        
    postMessage({
        diagnostics,
        id: e.data.id,
    });
};

postMessage("ready");