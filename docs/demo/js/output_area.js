export function initializeOutputArea(displayErrorsElem, displayLogElem, errorElem, logElem) {
    displayErrorsElem.addEventListener('click', e => {
        displayLogElem.classList.remove('selected');
        displayErrorsElem.classList.add('selected');
        logElem.style.display = 'none';
        errorElem.style.display = 'block';
    });
    displayLogElem.addEventListener('click', e => {
        displayLogElem.classList.add('selected');
        displayErrorsElem.classList.remove('selected');
        errorElem.style.display = 'none';
        logElem.style.display = 'block';
    });
}
export function updateErrorELement(displayErrorsElem, errorElem, errors, tokens) {
    displayErrorsElem.innerHTML = `Errors (${errors.length})`;
    errorElem.innerHTML = '';
    if (errors.length !== 0) {
        displayErrorsElem.click();
    }
    for (const error of errors) {
        const errorRowElem = document.createElement('div');
        const errorPosition = `Line: ${tokens[error.tokens[0]].line}, Col: ${tokens[error.tokens[0]].start}`;
        errorRowElem.innerText = `${error.error_type}: ${error.message} (${errorPosition})`;
        errorElem.appendChild(errorRowElem);
    }
}
export function updateLogELement(displayLogElem, logElem, interpreted) {
    logElem.innerText = interpreted.output;
    if (interpreted.output.length !== 0) {
        displayLogElem.click();
    }
}
