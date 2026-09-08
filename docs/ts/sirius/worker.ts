import compilerInit, { compile } from "./wasm/sirius";
import type { EditRequest, InitRequest, Diagnostic, LintResponse } from "./compiler";
import * as z3 from "./z3";

let initWasm = false;
let channel: z3.Channel | null = null;

onmessage = async (e: MessageEvent<EditRequest | InitRequest>) => {
    if("message" in e.data) {
        if(e.data.message == "poke") {
            const readyUp = () => {
                if("channel" in e.data) {
                    channel = e.data.channel;
                }
                postMessage({message: "ready"});
            }

            if(initWasm) {
                readyUp();
            } else {
                compilerInit().then(() => {
                    initWasm = true;
                    readyUp();
                })
            }
        }
        return;
    }

    if(!initWasm) {
        return;
    }

    try {
        function get_z3(smt2: string): string {
            if(channel === null) {
                throw new Error("Z3 channel not initialized");
            }

            return z3.runSync(channel, smt2, () => postMessage({message: "calling Z3"}));
        }

        const output = compile(e.data.code, get_z3);

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
