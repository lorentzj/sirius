import { CompositeAction, ActionHistory } from './action_history.js';
export class CodeLines {
    static InsertAction = class {
        lines;
        start;
        end;
        content;
        constructor(lines, start, content) {
            this.lines = lines;
            this.start = start;
            const newLines = content.split('\n');
            this.end = { line: start.line + newLines.length - 1, offset: 0 };
            if (newLines.length === 1) {
                this.end.offset = start.offset + newLines[0].length;
            }
            else {
                this.end.offset = newLines[newLines.length - 1].length;
            }
            this.content = content;
        }
        inverse() {
            return new CodeLines.DeleteAction(this.lines, this.start, this.end);
        }
        execute() {
            this.lines.insertWithoutRecord(this.start, this.content);
            return true;
        }
    };
    static DeleteAction = class {
        lines;
        start;
        end;
        content;
        constructor(lines, start, end) {
            this.lines = lines;
            this.start = start;
            this.end = end;
            this.content = lines.getText(start, end);
        }
        inverse() {
            return new CodeLines.InsertAction(this.lines, this.start, this.content);
        }
        execute() {
            this.lines.deleteWithoutRecord(this.start, this.end);
            return true;
        }
    };
    code;
    lastCaretPosition;
    history;
    constructor() {
        this.code = [''];
        this.lastCaretPosition = { line: 0, offset: 0 };
        this.history = new ActionHistory(50);
    }
    insertWithoutRecord(at, content) {
        this.lastCaretPosition.line = at.line;
        this.lastCaretPosition.offset = at.offset;
        const newLines = content.split('\n');
        const currLineBeforePaste = this.code[at.line].slice(0, at.offset);
        const currLineAfterPaste = this.code[at.line].slice(at.offset);
        this.code[at.line] = currLineBeforePaste + newLines[0];
        this.code.splice(at.line + 1, 0, ...newLines.slice(1));
        this.code[at.line + newLines.length - 1] += currLineAfterPaste;
        this.lastCaretPosition.line += newLines.length - 1;
        this.lastCaretPosition.offset = newLines[newLines.length - 1].length;
        if (newLines.length === 1) {
            this.lastCaretPosition.offset += at.offset;
        }
    }
    deleteWithoutRecord(from, to) {
        if (from.line === to.line && from.offset === to.offset) {
            return;
        }
        const beforeSlice = this.code[from.line].slice(0, from.offset);
        const afterSlice = this.code[to.line].slice(to.offset);
        this.code[from.line] = beforeSlice + afterSlice;
        this.code.splice(from.line + 1, to.line - from.line);
        if (this.code.length === 0) {
            this.code = [''];
        }
        this.lastCaretPosition.line = from.line;
        this.lastCaretPosition.offset = from.offset;
    }
    insert(at, content) {
        this.history.executeAction(new CodeLines.InsertAction(this, at, content));
    }
    delete(from, to) {
        this.history.executeAction(new CodeLines.DeleteAction(this, from, to));
    }
    deleteAndInsert(from, to, content) {
        this.history.executeAction(new CompositeAction([
            new CodeLines.DeleteAction(this, from, to),
            new CodeLines.InsertAction(this, from, content)
        ]));
    }
    getText(from, to) {
        if (from.line === to.line) {
            if (from.offset === to.offset) {
                return '';
            }
            else {
                return this.code[from.line].slice(from.offset, to.offset);
            }
        }
        else {
            let ret = [this.code[from.line].slice(from.offset, undefined)];
            let i = from.line + 1;
            while (i < to.line) {
                ret.push(this.code[i]);
                i += 1;
            }
            if (i < this.code.length) {
                ret.push(this.code[i].slice(undefined, to.offset));
            }
            return ret.join('\n');
        }
    }
    undo() {
        this.history.undoAction();
    }
    redo() {
        this.history.redoAction();
    }
}
