import { Spinner } from "./spinner.js";
import { Error } from "./error_list.js";
import { debounce, getElementIndex } from './utils.js';

export interface WorkerRequest {
    code: string;
    editId: number;
}

export type CompilerOutput = {
    errors: Error[];
    type_tokens: {
        line: number;
        start: number;
        end: number;
    }[];
}

export type WorkerResponse = {
    output: CompilerOutput;
    editId: number;
} | { ready: boolean };

export class Editor {
    private editor: HTMLDivElement;
    private lex: (code: string) => any[];
    private spinner: Spinner;
    private editId: number;
    private worker: Worker;
    private ready: boolean;
    private errorListeners: ((errors: Error[]) => void)[];

    constructor(id: string, compilerSpinnerId: string, lex: (code: string) => any[], worker: Worker) {
        this.worker = worker;
        this.spinner = new Spinner(compilerSpinnerId);
        this.lex = lex;
        this.editor = document.getElementById(id) as HTMLDivElement;
        this.editId = 0;
        this.ready = false;
        this.errorListeners = [];

        this.editor.contentEditable = 'true';
        this.editor.spellcheck = false;
        this.editor.addEventListener('input', this.onInput.bind(this));
        this.editor.addEventListener('keydown', this.onKeyDown.bind(this));
        this.editor.addEventListener('mousemove', debounce(this.onMouseMove.bind(this)));
        this.worker.onmessage = this.onMessage.bind(this);

        this.spinner.on();
        this.onInput(new Event('input'));
    }

    addErrorListener(listener: (errors: Error[]) => void) {
        this.errorListeners.push(listener);
    }

    private onMouseMove(event: MouseEvent) {
        const range = document.caretPositionFromPoint(event.clientX, event.clientY);
        if(range !== null && range.offsetNode instanceof Text) {
            const line = range.offsetNode.parentNode as HTMLElement;
            const lineNumber = getElementIndex(line);
            console.log(lineNumber, range.offset);
        }
    }

    private onMessage(event: MessageEvent<WorkerResponse>) {
        if('ready' in event.data) {
            if(event.data.ready === true) {
                this.ready = true;
                this.worker.postMessage({
                    code: this.getCode(),
                    editId: this.editId
                });
            }
        } else if(event.data.editId === this.editId) {
            this.spinner.off();

            const errorRanges = [];
            const typeTokenRanges = [];

            for(const error of event.data.output.errors) {
                errorRanges.push(...this.errorCodeRanges(error));
            }

            for(const typeToken of event.data.output.type_tokens) {
                typeTokenRanges.push(this.typeTokenCodeRange(typeToken));
            }

            const typeTokenHighlight = new Highlight(...typeTokenRanges);
            CSS.highlights.set('type-syntax', typeTokenHighlight);
            
            const errorHighlight = new Highlight(...errorRanges);
            CSS.highlights.set('error-syntax', errorHighlight);

            for(const listener of this.errorListeners) {
                listener(event.data.output.errors);
            }
        }
    }

    getCode() {
        const lines = this.editor.children;
        return [...lines].map(line => line.textContent).join('\n');
    }

    private onInput(event: Event) {
        const code = this.getCode();
        const tokens = this.lex(code);

        const highlights = {
            operator: [] as Range[],
            keyword: [] as Range[],
            number: [] as Range[],
            error: [] as Range[],
            comment: [] as Range[],
            type: [] as Range[]
        }

        for (const line of this.editor.children) {
            line.classList.remove('error');
        }

        type Token = {
            line: number;
            start: number;
            end: number;
            data: number;
            is_type_ann: boolean;
        }

        for(const token of tokens as Token[]) {
            const line = this.editor.children[token.line].childNodes[0];
            const range = new Range();
            range.setStart(line, token.start);
            range.setEnd(line, token.end);
            if(token.data === 1) {
                highlights.operator.push(range);
            } else if(token.data == 2) {
                highlights.number.push(range);
            } else if(token.data == 3) {
                highlights.keyword.push(range);
            } else if(token.data == 4) {
                this.editor.children[token.line].classList.add('error');
                highlights.error.push(range);
            } else if(token.data == 5) {
                highlights.comment.push(range);
            }
        }
    
        const operatorHighlights = new Highlight(...highlights.operator);
        const numberHighlights = new Highlight(...highlights.number);
        const keywordHighlights = new Highlight(...highlights.keyword);
        const errorHighlights = new Highlight(...highlights.error);
        const commentHighlights = new Highlight(...highlights.comment);
        const typeHighlights = new Highlight(...highlights.type);

        CSS.highlights.set('operator-syntax', operatorHighlights);
        CSS.highlights.set('number-syntax', numberHighlights);
        CSS.highlights.set('keyword-syntax', keywordHighlights);
        CSS.highlights.set('error-syntax', errorHighlights);
        CSS.highlights.set('comment-syntax', commentHighlights);
        CSS.highlights.set('type-syntax', typeHighlights);

        if(this.ready) {
            this.editId++;
            this.spinner.on();

            this.worker.postMessage({
                code: code,
                editId: this.editId
            });
        }
    }

    private typeTokenCodeRange(typeToken: { line: number; start: number; end: number }): Range {
        const lineTextNode = this.editor.children[typeToken.line].childNodes[0];
        const range = new Range();
        range.setStart(lineTextNode, typeToken.start);
        range.setEnd(lineTextNode, typeToken.end);
        return range;
    }

    private errorCodeRanges(error: Error): Range[] {
        const errorRanges: Range[] = [];
        const line = this.editor.children[error.start_line];
        line.classList.add('error');
    
        const lineTextNode = this.editor.children[error.start_line].childNodes[0];
        const firstRange = new Range();
        firstRange.setStart(lineTextNode, error.start_column);

        if(error.end_line === error.start_line) {
            firstRange.setEnd(lineTextNode, error.end_column);
        } else {
            firstRange.setEnd(lineTextNode, lineTextNode.textContent!.length);
        }

        errorRanges.push(firstRange);

        if(error.end_line !== error.start_line) {
            error.start_line++;

            const midRanges: Range[] = [];
            while(error.start_line < error.end_line) {
                const lineTextNode = this.editor.children[error.start_line].childNodes[0];
                const midRange = new Range();
                midRange.setStart(lineTextNode, 0);
                midRange.setEnd(lineTextNode, lineTextNode.textContent!.length);
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

    private onKeyDown(event: KeyboardEvent) {
        if(event.key === 'Backspace') {
            if(this.editor.children.length === 1 && this.editor.children[0].textContent === '') {
                event.preventDefault();
            }
        } else if(event.key === 'Tab') {
            event.preventDefault();
        }
    }
}