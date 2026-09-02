import compilerInit, { compile } from "./wasm/sirius";
import type { EditRequest, Poke, Diagnostic, LintResponse } from "./compiler";

let initWasm = false;

onmessage = async (e: MessageEvent<EditRequest | Poke>) => {
    if(!initWasm) {
        return;
    }

    if("message" in e.data) {
        postMessage({message: "ready"});
        return;
    }

    try {
        const output = compile(e.data.code);

        const lineOffsets = [0, ...[...e.data.code.matchAll(/\n/g)].map(match => match.index + 1)];
        const diagnostics: Diagnostic[] = [];

        for(const error of output.errors) {
            diagnostics.push({
                from: lineOffsets[error.start_line] + error.start_column,
                to: lineOffsets[error.end_line] + error.end_column,
                severity: "error",
                message: `${error.type}Error: ${error.message}`
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
        
        const message: LintResponse = {
            diagnostics,
            compiler_error: false,
            id: e.data.id,
        };

        postMessage(message);
    } catch (err) {
        const message: LintResponse = {
            diagnostics: [],
            compiler_error: true,
            id: e.data.id,
        };

        postMessage(message);
        return;
    }
};

compilerInit().then(() => {
    initWasm = true;
    postMessage({message: "ready"});
});