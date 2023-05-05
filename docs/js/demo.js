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
        const editorLinesElem = document.getElementById('editor_line_numbers');
        const buttonElem = document.getElementById('run');
        const displayErrorsElem = document.getElementById('display_errors');
        const displayLogElem = document.getElementById('display_log');
        const errorElem = document.getElementById('errors');
        const logElem = document.getElementById('log');
        const progSelectorElem = document.getElementById('demo_programs');
        let codeLines = new CodeLines();
        if (editorElem !== null
            && tooltipElem !== null
            && editorLinesElem !== null
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
            const updateEditor = () => {
                const parsed = JSON.parse(bindings.parse(codeLines.code.join('\n')));
                parsed.typeTokens = new Set(parsed.typeTokens);
                let hl_map = new Map();
                Object.keys(parsed.highlightMap).forEach(key => {
                    hl_map.set(Number.parseInt(key), parsed.highlightMap[key]);
                });
                parsed.highlightMap = hl_map;
                let ast = new Map();
                Object.keys(parsed.ast).forEach(key => {
                    ast.set(key, parsed.ast[key]);
                });
                parsed.ast = ast;
                console.log(ast);
                for (let fn of parsed.ast) {
                    console.log('------------------');
                    console.log(fn[0]);
                    for (let c of fn[1].constraints) {
                        console.log(types.constraint_name(c));
                    }
                    console.log('------------------');
                }
                editor.updateEditorWithCode(editorElem, tooltipElem, editorLinesElem, codeLines.code, parsed);
                editor.updateEditorWithErrors(parsed.errors, editorElem);
                editor.updateCaretPosition(codeLines.lastCaretPosition, editorElem);
                output_area.updateErrorELement(displayErrorsElem, errorElem, parsed.errors, parsed.tokens);
                return parsed;
            };
            buttonElem.addEventListener('click', _ => {
                const parsed = updateEditor();
                if (parsed.errors.length === 0) {
                    const interpreted = JSON.parse(bindings.interpret(codeLines.code.join('\n')));
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
