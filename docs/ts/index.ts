import { EditorView } from "codemirror";
import { history, historyKeymap, defaultKeymap } from "@codemirror/commands";
import { lineNumbers, keymap } from "@codemirror/view";
import { siriusHLLang, siriusHighlights } from "./sirius/highlight";
import { lintGutter } from "@codemirror/lint";
import { bracketMatching } from "@codemirror/language";
import { siriusLinter, highlightTypes, filterTypeInfo } from './sirius/compiler';

function main() {
    const extensions = [
        lineNumbers(),
        history(),
        keymap.of([...defaultKeymap, ...historyKeymap]),
        siriusHLLang,
        siriusHighlights,
        bracketMatching(),
        lintGutter({markerFilter: filterTypeInfo})
    ];

    for(const [i, editor] of document.querySelectorAll(".editor").entries()) {
        const code = editor.textContent.trimEnd();
        editor.textContent = "";
        new EditorView({
            parent: editor,
            doc: code,
            extensions: [siriusLinter(i), highlightTypes.of(i), ...extensions]
        });
    }
}

if (document.readyState === "complete") {
    main();
} else {
    document.addEventListener("DOMContentLoaded", main);
}