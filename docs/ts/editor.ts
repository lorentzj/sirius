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

    const editor = document.querySelector(".editor") as HTMLPreElement;
    const urlParams = new URLSearchParams(window.location.search);
    let code = editor.textContent.trimEnd();
    const codeParam = urlParams.get("code");
    if(codeParam !== null) {
        try {
            const binString = atob(codeParam)
            
            const len: number = binString.length;
            const bytes = new Uint8Array(len);
            for (let i = 0; i < len; i++) {
                bytes[i] = binString.charCodeAt(i);
            }

            code = new TextDecoder().decode(bytes);
        } catch (e) {
            console.error("failed to decode code param");

            console.error(e);
        }
    }
    
    editor.textContent = "";
    const editorView = new EditorView({
        parent: editor,
        doc: code,
        extensions: [siriusLinter(0), highlightTypes.of(0), ...extensions]
    });

    const linkButton = document.querySelector("#generate_link") as HTMLButtonElement;
    const linkCopied = document.querySelector("#link_copied") as HTMLSpanElement;

    linkButton.addEventListener("click", () => {
        const code = editorView.state.doc.toString();
        const bytes = new TextEncoder().encode(code);
        const binString = String.fromCharCode(...bytes);
        const b64Code = btoa(binString);
        let url = new URL(window.location.href);
        url.searchParams.set("code", b64Code);
        navigator.clipboard.writeText(url.toString());

        linkCopied.classList.remove('copied');

        requestAnimationFrame(() => {
            requestAnimationFrame(() => {
                linkCopied.classList.add('copied');

                const animations = linkCopied.getAnimations();
                if(animations.length > 0) {
                    animations[0].cancel();
                    requestAnimationFrame(() => {
                        animations[0].play();
                    });
                }
            });
        });
    });
}

if (document.readyState === "complete") {
    main();
} else {
    document.addEventListener("DOMContentLoaded", main);
}