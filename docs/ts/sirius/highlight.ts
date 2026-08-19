import { StreamLanguage } from "@codemirror/language";
import { HighlightStyle, syntaxHighlighting } from "@codemirror/language";
import { tags } from "@lezer/highlight";

const keywords = new Set([
    "print",
    "let",
    "if",
    "else",
    "true",
    "false",
    "and",
    "or",
    "fn",
    "return",
    "for",
    "from",
    "to",
    "\\in",
    "st",
    "yield",
    "mut",
    "null"
]);

export const siriusHLLang = StreamLanguage.define({
    token(stream) {
        if(stream.eatSpace()) return null;
        if(stream.match(/^\/\/.*/)) return "comment";

        if(stream.match(/^[a-zA-Z_\\][a-zA-Z0-9_\\]*/)) {
            if (keywords.has(stream.current())) {
                return "keyword";
            }
        } else {
            if(stream.match(/^\d+\.?\d*/)) {
                return "number";
            }
        }

        stream.next();
        return null;
    }
});

export const siriusHighlights = syntaxHighlighting(HighlightStyle.define([
    { tag: tags.comment, "class": "comment" },
    { tag: tags.keyword, "class": "keyword" },
    { tag: tags.number, "class": "number" }
]));