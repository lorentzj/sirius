import * as tooltip from './tooltip.js';
export function insertTab(editorElem, codeLines) {
    let selection = window.getSelection();
    if (selection !== null) {
        for (let i = 0; i < selection.rangeCount; ++i) {
            let targetRange = selection.getRangeAt(i);
            const insertPosition = codeIndexFromPosition(editorElem, targetRange.startContainer, targetRange.startOffset);
            codeLines.insert(insertPosition, '    ');
        }
    }
}
export function updateCodeLines(editorElem, codeLines, e) {
    e.preventDefault();
    for (const targetRange of e.getTargetRanges()) {
        const startPosition = codeIndexFromPosition(editorElem, targetRange.startContainer, targetRange.startOffset);
        const endPosition = codeIndexFromPosition(editorElem, targetRange.endContainer, targetRange.endOffset);
        const deleteOps = ['deleteContentBackward', 'deleteContentForward', 'deleteByCut', 'deleteByDrag'];
        if (e.inputType === 'insertText' && e.data !== null) {
            codeLines.delete(startPosition, endPosition);
            codeLines.insert(startPosition, e.data);
        }
        else if (e.inputType === 'insertParagraph' || e.inputType === 'insertLineBreak') {
            codeLines.insert(startPosition, '\n');
        }
        else if (deleteOps.indexOf(e.inputType) !== -1) {
            codeLines.delete(startPosition, endPosition);
        }
        else if ((e.inputType === 'insertFromPaste' || e.inputType === 'insertFromDrop') && e.dataTransfer !== null) {
            let pastedText = e.dataTransfer.getData('text/plain');
            codeLines.delete(startPosition, endPosition);
            codeLines.insert(startPosition, pastedText);
        }
        else {
            console.error(`No handler for ${e.inputType} event`);
        }
    }
    return codeLines;
}
export function updateCaretPosition(caretPosition, editorElem) {
    const selection = window.getSelection();
    if (selection !== null) {
        selection.removeAllRanges();
        let selectedRange = document.createRange();
        if (caretPosition.line === 0 && caretPosition.offset === 0) {
            selectedRange.setStart(editorElem, 0);
        }
        else {
            const lineSelector = `li.code_line[data-line='${caretPosition.line + 1}']`;
            const selectedLine = editorElem.querySelector(lineSelector);
            selectedLine.scrollIntoView();
            const firstElementIsSentinel = selectedLine.children[0].dataset['sentinel'] == 'true';
            if (firstElementIsSentinel) {
                selectedRange.setStart(selectedLine, 0);
            }
            else {
                for (let i = 0; i < selectedLine.children.length; ++i) {
                    const tokenElement = selectedLine.children[i];
                    const spanCharStart = Number.parseInt(tokenElement.dataset.charStart);
                    const spanCharEnd = Number.parseInt(tokenElement.dataset.charEnd);
                    if (spanCharStart <= caretPosition.offset && spanCharEnd >= caretPosition.offset) {
                        if (spanCharEnd === caretPosition.offset) {
                            selectedRange.setStartAfter(tokenElement);
                        }
                        else if (spanCharStart === caretPosition.offset) {
                            selectedRange.setStartBefore(tokenElement);
                        }
                        else {
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
function codeIndexFromPosition(editorElem, container, offset) {
    if (container.nodeType === Node.TEXT_NODE) {
        // inside token text node
        const token = container.parentElement;
        const line = token.parentElement;
        return {
            line: Number.parseInt(line.dataset.line) - 1,
            offset: Number.parseInt(token.dataset.charStart) + offset
        };
    }
    else if (container.classList.contains('code')) {
        // on edge of token
        const token = container;
        const line = token.parentElement;
        if (offset == 0) {
            return {
                line: Number.parseInt(line.dataset.line) - 1,
                offset: Number.parseInt(token.dataset.charStart)
            };
        }
        else {
            return {
                line: Number.parseInt(line.dataset.line) - 1,
                offset: Number.parseInt(token.dataset.charEnd)
            };
        }
    }
    else if (container.classList.contains('code_line')) {
        // between tokens in line
        const line = container;
        const lineNumber = Number.parseInt(line.dataset.line) - 1;
        const firstElementIsSentinel = line.children[0].dataset['sentinel'] == 'true';
        if (firstElementIsSentinel) {
            return {
                line: lineNumber,
                offset: 0
            };
        }
        else {
            const afterToken = line.children[offset - 1];
            if (afterToken === undefined) {
                return {
                    line: lineNumber,
                    offset: 0
                };
            }
            else {
                return {
                    line: lineNumber,
                    offset: Number.parseInt(afterToken.dataset['charEnd'])
                };
            }
        }
    }
    else {
        // between lines in editor
        const editor = container;
        // very start of editor, may be in editor container
        if (editor === editorElem) {
            if (offset === 0) {
                return { line: 0, offset: 0 };
            }
            else {
                const lastLine = editor.children[offset - 1];
                const lastToken = lastLine.children[lastLine.children.length - 1];
                const lastLineLen = Number.parseInt(lastToken.dataset['charEnd']);
                return { line: offset - 1, offset: lastLineLen };
            }
        }
        if (editor.childElementCount === 0) {
            return { line: 0, offset: 0 };
        }
        else {
            if (offset === editor.childElementCount) {
                const lastLine = editor.children[offset - 1];
                const lastToken = lastLine.children[lastLine.children.length - 1];
                const lastLineLen = Number.parseInt(lastToken.dataset['charEnd']);
                return { line: offset - 1, offset: lastLineLen };
            }
            else {
                return { line: offset, offset: 0 };
            }
        }
    }
}
export function updateEditorWithCode(editorElem, tooltipElem, code, parseOutput) {
    editorElem.innerHTML = '';
    let token_i = 0;
    code.forEach((lineCode, line) => {
        const lineElement = document.createElement('li');
        lineElement.classList.add('code_line');
        lineElement.dataset['line'] = (line + 1).toString();
        let line_i = 0;
        while (token_i < parseOutput.tokens.length && parseOutput.tokens[token_i].line === line) {
            const beforeTokenCode = lineCode.slice(line_i, parseOutput.tokens[token_i].start);
            if (beforeTokenCode.length > 0) {
                const beforeTokenCodeSpan = createCodeSpan(null, null, false);
                beforeTokenCodeSpan.textContent = beforeTokenCode;
                beforeTokenCodeSpan.dataset['charStart'] = line_i.toString();
                beforeTokenCodeSpan.dataset['charEnd'] = (line_i + beforeTokenCode.length).toString();
                lineElement.appendChild(beforeTokenCodeSpan);
            }
            const tokenCode = lineCode.slice(parseOutput.tokens[token_i].start, parseOutput.tokens[token_i].end);
            if (tokenCode.length > 0) {
                const tokenCodeSpan = createCodeSpan(parseOutput.tokens[token_i], token_i, parseOutput.typeTokens.has(token_i));
                setMouseOverHLHandler(editorElem, tokenCodeSpan, token_i, parseOutput.highlightMap);
                tokenCodeSpan.textContent = tokenCode;
                lineElement.appendChild(tokenCodeSpan);
                tooltip.setToolTipHandler(tooltipElem, tokenCodeSpan);
            }
            line_i = parseOutput.tokens[token_i].end;
            token_i += 1;
        }
        const afterTokenCode = lineCode.slice(line_i, undefined);
        if (afterTokenCode.length > 0) {
            const afterTokenCodeSpan = createCodeSpan(null, null, false);
            afterTokenCodeSpan.textContent = afterTokenCode;
            afterTokenCodeSpan.dataset['charStart'] = line_i.toString();
            afterTokenCodeSpan.dataset['charEnd'] = (line_i + afterTokenCode.length).toString();
            lineElement.appendChild(afterTokenCodeSpan);
        }
        if (!lineElement.hasChildNodes()) {
            lineElement.appendChild(document.createElement('br'));
            lineElement.children[0].dataset['sentinel'] = 'true';
        }
        editorElem.appendChild(lineElement);
    });
    tooltip.addTypeInfo(editorElem, parseOutput.ast);
    let errorLines = new Set();
    for (let error of parseOutput.errors) {
        const errorLine = parseOutput.tokens[error.start].line;
        const lineSelector = `li.code_line[data-line='${errorLine + 1}']`;
        const selectedLine = editorElem.querySelector(lineSelector);
        if (selectedLine !== null) {
            selectedLine.classList.add('error');
        }
    }
}
function setMouseOverHLHandler(editorElem, codeSpan, tokenId, highlightMap) {
    let highlightMatches = [];
    highlightMatches.push(...highlightMap.get(tokenId) || []);
    codeSpan.addEventListener('mouseenter', (_) => {
        highlightMatches.forEach(tokenId => {
            const tokenSelector = `span.token[data-token-id='${tokenId}']`;
            const tokenElem = editorElem.querySelector(tokenSelector);
            tokenElem?.classList.add('highlight');
        });
    });
    codeSpan.addEventListener('mouseleave', (_) => {
        highlightMatches.forEach(tokenId => {
            const tokenSelector = `span.token[data-token-id='${tokenId}']`;
            const tokenElem = editorElem.querySelector(tokenSelector);
            tokenElem?.classList.remove('highlight');
        });
    });
}
export function updateEditorWithErrors(errors, editorElem) {
    errors.forEach(error => {
        for (let tokenId = error.start; tokenId < error.end; tokenId++) {
            const tokenSelector = `span.token[data-token-id='${tokenId}']`;
            const tokenElem = editorElem.querySelector(tokenSelector);
            if (tokenElem !== null) {
                tokenElem.classList.add('error');
                tokenElem.dataset['error'] = error.message;
                tokenElem.dataset['errorType'] = `${error.error_type}Error`;
                if (tokenId + 1 < error.end) {
                    const nextElem = tokenElem.nextElementSibling;
                    if (nextElem !== null) {
                        nextElem.classList.add('error');
                        nextElem.dataset['error'] = error.message;
                        nextElem.dataset['errorType'] = `${error.error_type}Error`;
                    }
                }
            }
        }
    });
}
function createCodeSpan(token, i, isType) {
    const span = document.createElement('span');
    span.classList.add('code');
    if (i !== null) {
        span.dataset['tokenId'] = i.toString();
    }
    if (token !== null) {
        span.classList.add('token');
        span.dataset['charStart'] = token.start.toString();
        span.dataset['charEnd'] = token.end.toString();
        if (token.data.Op !== undefined) {
            span.classList.add('operator');
        }
        else if (token.data.Float !== undefined) {
            span.classList.add('const');
        }
        else if (token.data.Int !== undefined) {
            span.classList.add('const');
        }
        else if (token.data.Identifier !== undefined) {
            span.classList.add('identifier');
        }
        else if (token.data.Keyword !== undefined) {
            span.classList.add('keyword');
        }
        else if (token.data.Error !== undefined) {
            span.classList.add('error');
        }
        else if (token.data.IndentError !== undefined) {
            span.classList.add('error');
        }
        else {
            span.classList.add(token.data.toLowerCase());
        }
        if (isType) {
            span.classList.add('type');
        }
    }
    return span;
}
