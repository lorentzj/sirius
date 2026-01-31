import { Spinner } from "./spinner.js";
export class Editor {
    constructor(id, compilerSpinnerId, lex, worker) {
        this.worker = worker;
        this.spinner = new Spinner(compilerSpinnerId);
        this.lex = lex;
        this.editor = document.getElementById(id);
        this.editId = 0;
        this.ready = false;
        this.errorListeners = [];
        this.editor.contentEditable = 'true';
        this.editor.spellcheck = false;
        this.editor.addEventListener('input', this.onInput.bind(this));
        this.editor.addEventListener('keydown', this.onKeyDown.bind(this));
        this.worker.onmessage = this.onMessage.bind(this);
        this.spinner.on();
        this.onInput(new Event('input'));
    }
    addErrorListener(listener) {
        this.errorListeners.push(listener);
    }
    onMessage(event) {
        if ('ready' in event.data) {
            if (event.data.ready === true) {
                this.ready = true;
                this.worker.postMessage({
                    code: this.getCode(),
                    editId: this.editId
                });
            }
        }
        else if (event.data.editId === this.editId) {
            this.spinner.off();
            const errorRanges = [];
            for (const error of event.data.output.errors) {
                errorRanges.push(...this.errorCodeRanges(error));
            }
            const errorHighlight = new Highlight(...errorRanges);
            CSS.highlights.set('error-syntax', errorHighlight);
            for (const listener of this.errorListeners) {
                listener(event.data.output.errors);
            }
        }
    }
    getCode() {
        const lines = this.editor.children;
        return [...lines].map(line => line.textContent).join('\n');
    }
    onInput(event) {
        const code = this.getCode();
        const tokens = this.lex(code);
        const highlights = {
            operator: [],
            keyword: [],
            number: [],
            error: [],
            comment: []
        };
        for (const line of this.editor.children) {
            line.classList.remove('error');
        }
        for (const token of tokens) {
            const line = this.editor.children[token.line].childNodes[0];
            const range = new Range();
            range.setStart(line, token.start);
            range.setEnd(line, token.end);
            if (token.data === 1) {
                highlights.operator.push(range);
            }
            else if (token.data == 2) {
                highlights.number.push(range);
            }
            else if (token.data == 3) {
                highlights.keyword.push(range);
            }
            else if (token.data == 4) {
                this.editor.children[token.line].classList.add('error');
                highlights.error.push(range);
            }
            else if (token.data == 5) {
                highlights.comment.push(range);
            }
        }
        const operatorHighlights = new Highlight(...highlights.operator);
        const numberHighlights = new Highlight(...highlights.number);
        const keywordHighlights = new Highlight(...highlights.keyword);
        const errorHighlights = new Highlight(...highlights.error);
        const commentHighlights = new Highlight(...highlights.comment);
        CSS.highlights.set('operator-syntax', operatorHighlights);
        CSS.highlights.set('number-syntax', numberHighlights);
        CSS.highlights.set('keyword-syntax', keywordHighlights);
        CSS.highlights.set('error-syntax', errorHighlights);
        CSS.highlights.set('comment-syntax', commentHighlights);
        if (this.ready) {
            this.editId++;
            this.spinner.on();
            this.worker.postMessage({
                code: code,
                editId: this.editId
            });
        }
    }
    errorCodeRanges(error) {
        const errorRanges = [];
        const line = this.editor.children[error.start_line];
        line.classList.add('error');
        const lineTextNode = this.editor.children[error.start_line].childNodes[0];
        const firstRange = new Range();
        firstRange.setStart(lineTextNode, error.start_column);
        if (error.end_line === error.start_line) {
            firstRange.setEnd(lineTextNode, error.end_column);
        }
        else {
            firstRange.setEnd(lineTextNode, lineTextNode.textContent.length);
        }
        errorRanges.push(firstRange);
        if (error.end_line !== error.start_line) {
            error.start_line++;
            const midRanges = [];
            while (error.start_line < error.end_line) {
                const lineTextNode = this.editor.children[error.start_line].childNodes[0];
                const midRange = new Range();
                midRange.setStart(lineTextNode, 0);
                midRange.setEnd(lineTextNode, lineTextNode.textContent.length);
                midRanges.push(midRange);
                error.start_line++;
            }
            errorRanges.push(...midRanges);
            const lastRange = new Range();
            lastRange.setStart(line, 0);
            lastRange.setEnd(line, error.end_column);
            errorRanges.push(lastRange);
        }
        return errorRanges;
    }
    onKeyDown(event) {
        if (event.key === 'Backspace') {
            if (this.editor.children.length === 1 && this.editor.children[0].textContent === '') {
                event.preventDefault();
            }
        }
        else if (event.key === 'Tab') {
            event.preventDefault();
        }
    }
}
