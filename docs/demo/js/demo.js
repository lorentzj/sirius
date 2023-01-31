import init, * as bindings from '../js/sirius.js';
import * as editor from './editor.js';
import { CodeLines } from './code_lines.js';
import { load_demo_programs } from './demo_programs.js';
document.body.onload = () => {
    Promise.all([init(), load_demo_programs()]).then(([_, demo_programs]) => {
        const editorElem = document.getElementById('editor');
        const editorLinesElem = document.getElementById('editor_line_numbers');
        const buttonElem = document.getElementById('run');
        const errorElem = document.getElementById('errors');
        const logElem = document.getElementById('log');
        const progSelectorElem = document.getElementById('demo_programs');
        let codeLines = new CodeLines();
        if (editorElem !== null
            && editorLinesElem !== null
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
            const updateEditor = () => {
                const parsed = JSON.parse(bindings.parse(codeLines.code.join('\n')));
                editor.updateEditorWithCode(editorElem, editorLinesElem, codeLines.code, parsed);
                editor.updateEditorWithErrors(parsed.errors, editorElem);
                editor.updateCaretPosition(codeLines.lastCaretPosition, editorElem);
                updateErrorELement(errorElem, parsed.errors);
                return parsed;
            };
            buttonElem.addEventListener('click', _ => {
                const parsed = updateEditor();
                if (parsed.errors.length === 0) {
                    const interpreted = JSON.parse(bindings.interpret(codeLines.code.join('\n')));
                    logElem.innerText = interpreted.output;
                    if (interpreted.error) {
                        editor.updateEditorWithErrors([interpreted.error], editorElem);
                        updateErrorELement(errorElem, [interpreted.error]);
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
function updateErrorELement(errorElem, errors) {
    errorElem.innerHTML = '';
    for (const error of errors) {
        const errorRowElem = document.createElement('div');
        errorRowElem.innerText = error.error_type + ': ' + error.message;
        errorElem.appendChild(errorRowElem);
    }
}
