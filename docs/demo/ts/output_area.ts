import * as types from './types.js';

export function initializeOutputArea(displayErrorsElem: HTMLElement, displayLogElem: HTMLElement, errorElem: HTMLElement, logElem: HTMLElement) {
    (displayErrorsElem as HTMLButtonElement).addEventListener('click', e => {
        displayLogElem.classList.remove('selected');
        displayErrorsElem.classList.add('selected');
        logElem.style.display = 'none';
        errorElem.style.display = 'block';
    });
    (displayLogElem as HTMLButtonElement).addEventListener('click', e => {
        displayLogElem.classList.add('selected');
        displayErrorsElem.classList.remove('selected');

        errorElem.style.display = 'none';
        logElem.style.display = 'block';
    });
}

export function updateErrorELement(displayErrorsElem: HTMLElement, errorElem: HTMLElement, errors: types.Error[], tokens: types.Token[]) {
    displayErrorsElem.innerHTML = `Errors (${errors.length})`;
    errorElem.innerHTML = '';
    if(errors.length !== 0) {
        displayErrorsElem.click();
    }

    for(const error of errors) {
        const errorRowElem = document.createElement('div');
        const errorPosition = `Ln ${tokens[error.tokens[0]].line + 1}`;
        errorRowElem.innerText = `${error.error_type}: ${error.message} [${errorPosition}]`;
        errorElem.appendChild(errorRowElem);
    }
}

export function updateLogELement(displayLogElem: HTMLElement, logElem: HTMLElement, interpreted: types.InterpreterOutput) {
    logElem.innerText = interpreted.output;
    if(interpreted.output.length !== 0) {
        displayLogElem.click();
    }
}