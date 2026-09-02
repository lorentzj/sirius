import { linter, Diagnostic, setDiagnosticsEffect } from "@codemirror/lint";
import { EditorView, Decoration, ViewPlugin, ViewUpdate, DecorationSet } from "@codemirror/view";
import { RangeSetBuilder } from "@codemirror/state";
export { Diagnostic } from "@codemirror/lint";

import * as z3 from "./z3";

export type EditRequest = {
    id: string,
    code: string
}

export type CheckReady = {
    message: "poke",
    channel: z3.Channel
}

export type LintResponse = {
    id: string,
    diagnostics: Diagnostic[],
    compiler_error: boolean,
    message?: string
}

let worker: Worker | null = null;
let z3Channel = z3.channel(128*1024);

const decorationData: Map<number, Diagnostic[]> = new Map();

async function getWorker(status: StatusLine): Promise<Worker> {
    if(worker === null) {
        status.loading("loading compiler");
        z3.processWhenAny(z3Channel, 100);
        worker = new Worker(new URL("./worker.ts", import.meta.url), { type: "module" });
    }

    const p = new Promise<Worker>((resolve) => {        
        function handleReady(event: MessageEvent) {
            if (event.data.message === "ready" && worker !== null) {
                worker.removeEventListener("message", handleReady);
                status.finished(false);
                resolve(worker); 
            } else if(event.data.message !== undefined) {
                status.loading(event.data.message);
            }
        };
        if(worker !== null) {
            worker.addEventListener("message", handleReady);
        }
    });

    worker.postMessage({message: "poke", channel: z3Channel} as CheckReady);

    return p;
}

export function filterTypeInfo(diagnostics: readonly Diagnostic[]): Diagnostic[] {
    return diagnostics.filter((v) => v.severity != "info");
}

export const siriusLinter = (editorId: number) => {
    let editNumber = 0;
    decorationData.set(editorId, []);

    const compilerStatus = new StatusLine();

    return linter(async (view) => {
        if(!compilerStatus.root.isConnected) {
            view.dom.insertAdjacentElement("afterend", compilerStatus.root);
        }

        editNumber += 1;
        const editId = `${editorId}-${editNumber}`;

        let worker = await getWorker(compilerStatus);

        return new Promise((resolve) => {
            worker.addEventListener("message", function listener(event: MessageEvent<LintResponse>) {
                if(event.data.id === editId) {
                    worker.removeEventListener("message", listener);
                    decorationData.set(
                        editorId,
                        event.data.diagnostics.filter((e) => e.severity === "info"
                    ));
                    compilerStatus.finished(event.data.compiler_error);
                    resolve(event.data.diagnostics);
                }
            });

            const message = {
                id: editId,
                code: view.state.doc.toString()
            };

            compilerStatus.loading("compiling");
            worker.postMessage(message);
        });
    }, {
        tooltipFilter: filterTypeInfo,
        markerFilter: filterTypeInfo,
        autoPanel: true,
        delay: 50
    });
}

const typeDecoration = Decoration.mark({ class: "type" });

class TypeHighlights {
    decorations: DecorationSet;
    editorId: number

    constructor(view: EditorView, editorId: number) {
        this.editorId = editorId;
        this.decorations = this.buildDecorations(view);
    }

    update(update: ViewUpdate) {
        const lintChanged = update.transactions.some((tr) =>
            tr.effects.some((effect) => effect.is(setDiagnosticsEffect))
        );

        if(update.viewportChanged || lintChanged) {
            this.decorations = this.buildDecorations(update.view);
        }
    }

    buildDecorations(view: EditorView): DecorationSet {
        const builder = new RangeSetBuilder<Decoration>();
        for (let { from, to } of view.visibleRanges) {
            for (const diagnostic of decorationData.get(this.editorId) || []) {
                if(diagnostic.from <= to && diagnostic.to >= from && diagnostic.severity === "info") {
                    builder.add(diagnostic.from, diagnostic.to, typeDecoration);
                }
            }
        }

        return builder.finish();
    }
}

export const highlightTypes = ViewPlugin.fromClass(TypeHighlights, {
    decorations: v => v.decorations
});

class StatusLine {
    root: HTMLDivElement;
    private compilerLoader: HTMLSpanElement;
    private compilerMessage: HTMLSpanElement;

    constructor() {
        this.root = document.createElement("div");
        this.root.classList.add("compiler_status");

        this.compilerLoader = document.createElement("span");
        this.compilerMessage = document.createElement("span");
        this.compilerLoader.classList.add("loader");

        this.loading("loading");
        this.root.appendChild(this.compilerLoader);
        this.root.appendChild(this.compilerMessage);
    }

    finished(compiler_error: boolean) {
        this.compilerLoader.classList.remove("loading");
        this.root.classList.remove("loading");

        if(compiler_error) {
            this.root.classList.add("error");
            this.compilerMessage.textContent = "compiler crashed";
        } else {
            this.compilerMessage.textContent = "idle";
        }
    }

    loading(message: string | undefined) {
        if(message !== undefined) {
            this.compilerMessage.textContent = message;
        } 

        this.root.classList.remove("error");
        this.compilerLoader.classList.add("loading");
        this.root.classList.add("loading");
    }
}