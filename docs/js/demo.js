import init, * as bindings from '../js/sirius.js';
import * as blockConstraints from './block_constraints.js';
import * as editor from './editor.js';
import * as types from './types.js';
import * as outputArea from './output_area.js';
import { CodeLines } from './code_lines.js';
import { load_demo_programs } from './demo_programs.js';
document.body.onload = () => {
    const parserDelayMs = 0;
    let runId = 0;
    Promise.all([init(), load_demo_programs()]).then(([_, demo_programs]) => {
        const editorElem = document.getElementById('editor');
        const blockConstraintsElem = document.getElementById('block_constraints');
        const tooltipElem = document.getElementById('tooltip');
        const runButtonElem = document.getElementById('run');
        const displayErrorsElem = document.getElementById('display_errors');
        const displayLogElem = document.getElementById('display_log');
        const errorElem = document.getElementById('errors');
        const logElem = document.getElementById('log');
        const progSelectorElem = document.getElementById('demo_programs');
        const compilerRunningSpinnerElem = document.getElementById('compiler_running_spinner');
        let codeLines = new CodeLines();
        if (!window.Worker) {
            console.error('No WebWorkers!');
        }
        const currURL = new URL(import.meta.url);
        const currURLPathPieces = currURL.pathname.split('/');
        const currURLParentParent = currURLPathPieces.slice(undefined, currURLPathPieces.length - 2).join('/');
        const parserWorker = new Worker(`${currURLParentParent}/js/parser_worker.js`, { 'type': 'module' });
        const interpreterWorker = new Worker(`${currURLParentParent}/js/interpreter_worker.js`, { 'type': 'module' });
        if (editorElem !== null
            && tooltipElem !== null
            && displayErrorsElem !== null
            && displayLogElem !== null
            && errorElem !== null
            && logElem !== null
            && runButtonElem !== null
            && progSelectorElem !== null
            && blockConstraintsElem !== null
            && compilerRunningSpinnerElem !== null) {
            for (const demo_program in demo_programs) {
                const programOption = document.createElement('option');
                programOption.value = demo_program;
                programOption.text = demo_program;
                progSelectorElem.appendChild(programOption);
            }
            progSelectorElem.addEventListener('input', _ => {
                const value = progSelectorElem.value;
                codeLines.lastCaretPosition = { line: 0, offset: 0 };
                codeLines.delete({ line: 0, offset: 0 }, {
                    line: codeLines.code.length - 1,
                    offset: codeLines.code[codeLines.code.length - 1].length
                });
                if (value !== 'empty') {
                    codeLines.insert({ line: 0, offset: 0 }, demo_programs[value]);
                }
                updateEditor(false, parserDelayMs);
            });
            outputArea.initializeOutputArea(displayErrorsElem, displayLogElem, errorElem, logElem);
            editorElem.addEventListener('keydown', e => {
                if (e.key === 'Tab') {
                    editor.insertTab(editorElem, codeLines);
                    updateEditor(false, parserDelayMs);
                    e.preventDefault();
                }
            });
            const updateEditor = (andExecute, parserDelay) => {
                parserWorker.postMessage({
                    code: codeLines.toString(),
                    id: codeLines.editId,
                    andExecute: andExecute,
                    delayMs: parserDelay
                });
                compilerRunningSpinnerElem.dataset['active'] = 'true';
                const lex_output = types.prepare_parse_output(bindings.lex(codeLines.toString()));
                const scrollTop = editorElem.parentElement.scrollTop;
                const scrollBottom = scrollTop + editorElem.parentElement.clientHeight;
                editor.updateEditorWithCode(editorElem, tooltipElem, codeLines.code, lex_output);
                editor.updateEditorWithErrors(lex_output.errors, editorElem);
                editor.updateCaretPosition(codeLines.lastCaretPosition, editorElem, scrollTop, scrollBottom);
                outputArea.updateErrorElement(displayErrorsElem, errorElem, lex_output.errors, lex_output.tokens);
            };
            parserWorker.onmessage = (e) => {
                if (e.data.id == codeLines.editId) {
                    const scrollTop = editorElem.parentElement.scrollTop;
                    const scrollBottom = scrollTop + editorElem.parentElement.clientHeight;
                    editor.updateEditorWithCode(editorElem, tooltipElem, codeLines.code, e.data.parsed);
                    editor.updateEditorWithErrors(e.data.parsed.errors, editorElem);
                    blockConstraints.updateBlockConstraintsElem(blockConstraintsElem, editorElem, e.data.parsed);
                    editor.updateCaretPosition(codeLines.lastCaretPosition, editorElem, scrollTop, scrollBottom);
                    outputArea.updateErrorElement(displayErrorsElem, errorElem, e.data.parsed.errors, e.data.parsed.tokens);
                    if (e.data.andExecute && e.data.parsed.errors.length === 0) {
                        interpreterWorker.postMessage({
                            code: codeLines.toString(),
                            id: runId,
                            tokens: e.data.parsed.tokens
                        });
                    }
                    else {
                        compilerRunningSpinnerElem.dataset['active'] = 'false';
                    }
                }
            };
            interpreterWorker.onmessage = (e) => {
                if (e.data.id === runId) {
                    outputArea.updateLogELement(displayLogElem, logElem, e.data.output);
                    if (e.data.output.error) {
                        editor.updateEditorWithErrors([e.data.output.error], editorElem);
                        outputArea.updateErrorElement(displayErrorsElem, errorElem, [e.data.output.error], e.data.tokens);
                    }
                    compilerRunningSpinnerElem.dataset['active'] = 'false';
                }
            };
            runButtonElem.addEventListener('click', _ => {
                runId += 1;
                updateEditor(true, 0);
            });
            document.addEventListener('keydown', event => {
                if (event.ctrlKey && event.key === 'Enter') {
                    runButtonElem.click();
                }
                if (event.ctrlKey && event.key.toLocaleLowerCase() === 'z') {
                    if (event.shiftKey) {
                        codeLines.redo();
                        updateEditor(false, parserDelayMs);
                    }
                    else {
                        codeLines.undo();
                        updateEditor(false, parserDelayMs);
                    }
                }
            });
            editorElem.addEventListener('beforeinput', event => {
                editor.updateCodeLines(editorElem, codeLines, event);
                updateEditor(false, parserDelayMs);
            });
        }
    });
};
