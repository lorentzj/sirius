import init, * as bindings from '../js/sirius.js';
import * as editor from './editor.js';
import * as types from './types.js';
import * as output_area from './output_area.js';
import { CodeLines } from './code_lines.js';
import { load_demo_programs } from './demo_programs.js';
document.body.onload = () => {
    Promise.all([init(), load_demo_programs()]).then(([_, demo_programs]) => {
        const editorElem = document.getElementById('editor');
        const tooltipElem = document.getElementById('tooltip');
        const buttonElem = document.getElementById('run');
        const displayErrorsElem = document.getElementById('display_errors');
        const displayLogElem = document.getElementById('display_log');
        const errorElem = document.getElementById('errors');
        const logElem = document.getElementById('log');
        const progSelectorElem = document.getElementById('demo_programs');
        let codeLines = new CodeLines();
        if (editorElem !== null
            && tooltipElem !== null
            && displayErrorsElem !== null
            && displayLogElem !== null
            && errorElem !== null
            && logElem !== null
            && buttonElem !== null
            && progSelectorElem !== null) {
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
                updateEditor();
            });
            output_area.initializeOutputArea(displayErrorsElem, displayLogElem, errorElem, logElem);
            editorElem.addEventListener('keydown', e => {
                if (e.key === 'Tab') {
                    editor.insertTab(editorElem, codeLines);
                    updateEditor();
                    e.preventDefault();
                }
            });
            const updateEditor = () => {
                const parser_output = types.prepare_parse_output(bindings.parse(codeLines.code.join('\n') + '\n'));
                const scrollTop = editorElem.parentElement.scrollTop;
                const scrollBottom = scrollTop + editorElem.parentElement.clientHeight;
                editor.updateEditorWithCode(editorElem, tooltipElem, codeLines.code, parser_output);
                editor.updateEditorWithErrors(parser_output.errors, editorElem);
                editor.updateCaretPosition(codeLines.lastCaretPosition, editorElem, scrollTop, scrollBottom);
                output_area.updateErrorELement(displayErrorsElem, errorElem, parser_output.errors, parser_output.tokens);
                return parser_output;
            };
            buttonElem.addEventListener('click', _ => {
                const parsed = updateEditor();
                if (parsed.errors.length === 0) {
                    const interpreted = JSON.parse(bindings.interpret(codeLines.code.join('\n') + '\n'));
                    output_area.updateLogELement(displayLogElem, logElem, interpreted);
                    if (interpreted.error) {
                        editor.updateEditorWithErrors([interpreted.error], editorElem);
                        output_area.updateErrorELement(displayErrorsElem, errorElem, [interpreted.error], parsed.tokens);
                    }
                }
            });
            document.addEventListener('keydown', event => {
                if (event.ctrlKey && event.key === 'Enter') {
                    buttonElem.click();
                }
                if (event.ctrlKey && event.key.toLocaleLowerCase() === 'z') {
                    if (event.shiftKey) {
                        codeLines.redo();
                        updateEditor();
                    }
                    else {
                        codeLines.undo();
                        updateEditor();
                    }
                }
            });
            editorElem.addEventListener('beforeinput', event => {
                editor.updateCodeLines(editorElem, codeLines, event);
                updateEditor();
            });
        }
    });
};
