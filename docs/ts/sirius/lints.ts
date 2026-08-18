import { linter, Diagnostic, setDiagnosticsEffect } from "@codemirror/lint";
import { EditorView, Decoration, ViewPlugin, ViewUpdate, DecorationSet } from "@codemirror/view";
import { RangeSetBuilder } from "@codemirror/state";

export { Diagnostic } from "@codemirror/lint";


export type EditRequest = {
    id: string,
    code: string
}

export type LintResponse = {
    id: string,
    diagnostics: Diagnostic[]
}

let worker: Worker | null = null;
const decorationData: Map<number, Diagnostic[]> = new Map();

function getWorker(): Promise<Worker> {
    return new Promise((resolve) => {
        if(worker !== null) {
            resolve(worker);
        } else {
            worker = new Worker(new URL("./worker.ts", import.meta.url), { type: "module" });
            worker.onmessage = function handleReady(event) {
                if (event.data === "ready" && worker !== null) {
                    worker.removeEventListener("message", handleReady);
                    resolve(worker); 
                }
            };
        }
    });
}

export function filterTypeInfo(diagnostics: readonly Diagnostic[]): Diagnostic[] {
    return diagnostics.filter((v) => v.severity != "info");
}

export const siriusLinter = (editorId: number) => {
    let editNumber = 0;
    decorationData.set(editorId, []);

    return linter(async (view) => {
        editNumber += 1;
        const editId = `${editorId}-${editNumber}`;

        let worker = await getWorker();

        return new Promise((resolve) => {
            worker.addEventListener("message", function listener(event: MessageEvent<LintResponse>) {
                if(event.data.id === editId) {
                    worker.removeEventListener("message", listener);
                    decorationData.set(
                        editorId,
                        event.data.diagnostics.filter((e) => e.severity === "info"
                    ));
                    resolve(event.data.diagnostics);
                }
            });

            const message = {
                id: editId,
                code: view.state.doc.toString()
            };

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