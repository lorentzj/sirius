import * as types from './types.js';
import {CodeLines, CodePosition} from './code_lines.js'

export function updateCodeLines(editorElem: HTMLElement, codeLines: CodeLines, e: InputEvent) {
    e.preventDefault();

    for(const targetRange of e.getTargetRanges()) {
        const startPosition = codeIndexFromPosition(editorElem, targetRange.startContainer, targetRange.startOffset);
        const endPosition = codeIndexFromPosition(editorElem, targetRange.endContainer, targetRange.endOffset);

        const deleteOps = ['deleteContentBackward', 'deleteContentForward', 'deleteByCut', 'deleteByDrag']

        if(e.inputType === 'insertText' && e.data !== null) {
            codeLines.delete(startPosition, endPosition);
            codeLines.insert(startPosition, e.data);
        } else if(e.inputType === 'insertParagraph' || e.inputType === 'insertLineBreak') {
            codeLines.insert(startPosition, '\n');
        } else if(deleteOps.indexOf(e.inputType) !== -1) {
            codeLines.delete(startPosition, endPosition);
        } else if((e.inputType === 'insertFromPaste'|| e.inputType === 'insertFromDrop') && e.dataTransfer !== null) {
            let pastedText = e.dataTransfer.getData('text/plain');
            codeLines.delete(startPosition, endPosition);
            codeLines.insert(startPosition, pastedText);
        } else {
            console.error(`No handler for ${e.inputType} event`);
        }
    }

    return codeLines;
}

export function updateCaretPosition(caretPosition: CodePosition, editorElem: HTMLElement) {

    const selection = window.getSelection();
    if(selection !== null) {
        selection.removeAllRanges();
        
        let selectedRange = document.createRange();
        if(caretPosition.line === 0 && caretPosition.offset === 0) {
            selectedRange.setStart(editorElem, 0);
        } else {
            const lineSelector = `div.code_line[data-line='${caretPosition.line + 1}']`;
            const selectedLine = editorElem.querySelector(lineSelector) as HTMLElement;

            selectedLine.scrollIntoView();

            const firstElementIsSentinel = (selectedLine.children[0] as HTMLElement).dataset['sentinel'] == 'true';

            if(firstElementIsSentinel) {
                selectedRange.setStart(selectedLine, 0);
            } else {
                for(let i = 0; i < selectedLine.children.length; ++i) {
                    const tokenElement  = (selectedLine.children[i] as HTMLElement);
                    const spanCharStart = Number.parseInt(tokenElement.dataset.charStart as string);
                    const spanCharEnd   = Number.parseInt(tokenElement.dataset.charEnd as string);
                    if(spanCharStart <= caretPosition.offset && spanCharEnd >= caretPosition.offset) {
                        if(spanCharEnd === caretPosition.offset) {
                            selectedRange.setStartAfter(tokenElement);  
                        } else if(spanCharStart === caretPosition.offset) {
                            selectedRange.setStartBefore(tokenElement);  
                        } else {
                            selectedRange.setStart(tokenElement.childNodes[0], caretPosition.offset - spanCharStart);
                        }
                        break;
                    }
                }
            }        
        }
        selection.addRange(selectedRange);
    }
}

function codeIndexFromPosition(editorElem: HTMLElement, container: Node, offset: number): CodePosition {
    if(container.nodeType === Node.TEXT_NODE) {
        // inside token text node
        const token = container.parentElement as HTMLElement;
        const line = token.parentElement as HTMLElement;
        return {
            line: Number.parseInt(line.dataset.line as string) - 1,
            offset: Number.parseInt(token.dataset.charStart as string) + offset
        }

    } else if((container as HTMLElement).classList.contains('code')) {

        // on edge of token
        const token = container as HTMLElement;
        const line = token.parentElement as HTMLElement;

        if(offset == 0) {
            return {
                line: Number.parseInt(line.dataset.line as string) - 1,
                offset: Number.parseInt(token.dataset.charStart as string)
            };
        } else {
            return {
                line: Number.parseInt(line.dataset.line as string) - 1,
                offset: Number.parseInt(token.dataset.charEnd as string)
            };
        }
    } else if((container as HTMLElement).classList.contains('code_line')) {
        // between tokens in line
        const line = container as HTMLElement;
        const lineNumber = Number.parseInt(line.dataset.line as string) - 1;

        const firstElementIsSentinel = (line.children[0] as HTMLElement).dataset['sentinel'] == 'true';
        
        if(firstElementIsSentinel) {
            return {
                line: lineNumber,
                offset: 0
            };
        } else {
            const afterToken = line.children[offset - 1] as HTMLElement;
            if(afterToken === undefined) {
                return {
                    line: lineNumber,
                    offset: 0
                };    
            } else {
                return {
                    line: lineNumber,
                    offset: Number.parseInt(afterToken.dataset['charEnd'] as string)
                };    
            }
        }
    } else {
        // between lines in editor
        const editor = container as HTMLElement;
        // very start of editor, may be in editor container
        if(editor === editorElem) {
            if(offset === 0) {
                return {line: 0, offset: 0};
            } else {
                const lastLine = editor.children[offset - 1];
    
                const lastToken = lastLine.children[lastLine.children.length - 1] as HTMLElement;
                const lastLineLen = Number.parseInt(lastToken.dataset['charEnd'] as string);
                return {line: offset - 1, offset: lastLineLen};
            }
        }

        if(editor.childElementCount === 0) {
            return {line: 0, offset: 0};
        } else {
            if(offset === editor.childElementCount) {
                const lastLine = editor.children[offset - 1];
    
                const lastToken = lastLine.children[lastLine.children.length - 1] as HTMLElement;
                const lastLineLen = Number.parseInt(lastToken.dataset['charEnd'] as string);
                return {line: offset - 1, offset: lastLineLen};
            } else {
                return {line: offset, offset: 0};
            }
        }
    }
}

export function updateEditorWithCode(editorElem: HTMLElement, editorLinesElem: HTMLElement, code: string[], parseOutput: types.ParserOutput) {
    editorElem.innerHTML = '';
    let token_i = 0;

    code.forEach((lineCode, line) => {
        const lineElement = document.createElement('div');
        lineElement.classList.add('code_line');
        lineElement.dataset['line'] = (line + 1).toString();

        let line_i = 0;
        while(token_i < parseOutput.tokens.length && parseOutput.tokens[token_i].line === line) {
            const before_token_code = lineCode.slice(line_i, parseOutput.tokens[token_i].start);
            if(before_token_code.length > 0) {
                const before_token_code_span = createCodeSpan(null, null, false, undefined);
                before_token_code_span.textContent = before_token_code;
                before_token_code_span.dataset['charStart'] = line_i.toString();
                before_token_code_span.dataset['charEnd'] = (line_i + before_token_code.length).toString();
                lineElement.appendChild(before_token_code_span);
            }

            const token_code = lineCode.slice(parseOutput.tokens[token_i].start, parseOutput.tokens[token_i].end);
            const token_code_span = createCodeSpan(
                parseOutput.tokens[token_i],
                token_i,
                parseOutput.typeTokens.has(token_i),
                parseOutput.annotations.get(token_i)
            );

            setMouseOverHandler(editorElem, token_code_span, token_i, parseOutput.highlightMap);
            token_code_span.textContent = token_code;
            lineElement.appendChild(token_code_span);

            line_i = parseOutput.tokens[token_i].end;
            token_i += 1;
        }

        const after_token_code = lineCode.slice(line_i, undefined);
        if(after_token_code.length > 0) {
            const after_token_code_span = createCodeSpan(null, null, false, undefined);
            after_token_code_span.textContent = after_token_code;
            after_token_code_span.dataset['charStart'] = line_i.toString();
            after_token_code_span.dataset['charEnd'] = (line_i + after_token_code.length).toString();
            lineElement.appendChild(after_token_code_span);
        }

        if(!lineElement.hasChildNodes()) {
            lineElement.appendChild(document.createElement('br'));
            (lineElement.children[0] as HTMLElement).dataset['sentinel'] = 'true';
        }
        editorElem.appendChild(lineElement);
    });

    let lines = [];
    for(let i = 1; i < code.length; ++i) {
        lines.push(`${i + 1}`);
    }
    editorLinesElem.innerHTML = '1\n' + lines.join('\n');
}

function setMouseOverHandler(editorElem: HTMLElement, codeSpan: HTMLElement, tokenId: number, highlightMap: Map<number, number[]>) {
    let highlightMatches: number[] = [];
    highlightMatches.push(...highlightMap.get(tokenId) || []);

    codeSpan.addEventListener('mouseenter', (_) => {
        highlightMatches.forEach(tokenId => {
            const tokenSelector = `span.token[data-token-id='${tokenId}']`;
            const tokenElem = editorElem.querySelector(tokenSelector);
            tokenElem?.classList.add('highlight');
        })
    });

    codeSpan.addEventListener('mouseleave', (_) => {
        highlightMatches.forEach(tokenId => {
            const tokenSelector = `span.token[data-token-id='${tokenId}']`;
            const tokenElem = editorElem.querySelector(tokenSelector);
            tokenElem?.classList.remove('highlight');
        })
    });
}

export function updateEditorWithErrors(errors: types.Error[], editorElem: HTMLElement) {
    errors.forEach(error => {
        for(let tokenId = error.start; tokenId < error.end; tokenId++) {
            const tokenSelector = `span.token[data-token-id='${tokenId}']`;
            const tokenElem = editorElem.querySelector(tokenSelector);
            if(tokenElem !== null) {
                tokenElem.classList.add('error');
                (tokenElem as HTMLElement).title = `${error.error_type}: ${error.message}`;
                if(tokenId + 1 < error.end) {
                    const nextElem = tokenElem.nextElementSibling;
                    if(nextElem !== null) {
                        nextElem.classList.add('error');
                        (nextElem as HTMLElement).title = `${error.error_type}: ${error.message}`;        
                    }
                }
            }
        }
    });
}

function createCodeSpan(token: types.Token | null, i: number | null, isType: boolean, annotation: string | undefined): HTMLElement {
    const span = document.createElement('span');
    span.classList.add('code');
    if(i !== null) {
        span.dataset['tokenId'] = i.toString();
    }
    if(token !== null) {
        span.classList.add('token');
        span.dataset['charStart'] = token.start.toString();
        span.dataset['charEnd'] = token.end.toString();
        if((token.data as {Op: string}).Op !== undefined) {
            span.classList.add('operator');
        } else if((token.data as {Float: number}).Float !== undefined) {
            span.classList.add('const');
        } else if((token.data as {Int: number}).Int !== undefined) { 
            span.classList.add('const');            
        } else if((token.data as {Identifier: string}).Identifier !== undefined) {
            span.classList.add('identifier');
        } else if((token.data as {Keyword: string}).Keyword !== undefined) {
            span.classList.add('keyword');
        } else if((token.data as {Error: string}).Error !== undefined) {
            span.classList.add('error');
        } else {
            span.classList.add((token.data as string).toLowerCase());
        }
        if(isType) {
            span.classList.add('type');
        }
    }

    if(annotation !== undefined) {
        span.title = annotation;
    }

    return span;
}