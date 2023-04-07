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
function parseErrorMessage(errorMessage) {
    const messageElement = document.createElement('span');
    const codePattern = /"(.*?)"/sg;
    let match = codePattern.exec(errorMessage);
    let i = 0;
    while (match !== null) {
        const messageCodeElement = document.createElement('span');
        messageCodeElement.innerText = match[1];
        messageCodeElement.classList.add('error_message_code');
        messageElement.appendChild(document.createTextNode(errorMessage.substring(i, match.index)));
        messageElement.appendChild(messageCodeElement);
        i = match.index + match[0].length;
        match = codePattern.exec(errorMessage);
    }
    messageElement.appendChild(document.createTextNode(errorMessage.substring(i)));
    return messageElement;
}
export function updateErrorELement(displayErrorsElem, errorElem, errors, tokens) {
    displayErrorsElem.innerHTML = `Errors (${errors.length})`;
    errorElem.innerHTML = '';
    if (errors.length !== 0) {
        displayErrorsElem.click();
    }
    errors = errors.sort((a, b) => {
        const aErrorLine = tokens[a.start].line + 1;
        const bErrorLine = tokens[b.start].line + 1;
        return aErrorLine - bErrorLine;
    });
    for (const error of errors) {
        const errorRowElem = document.createElement('details');
        errorRowElem.classList.add('error_row');
        const errorRowSummaryElem = document.createElement('summary');
        errorRowSummaryElem.classList.add('error_summary');
        const errorPositionElement = document.createElement('span');
        if (error.start < tokens.length) {
            errorPositionElement.innerText = `[Ln ${tokens[error.start].line + 1}]`;
        }
        errorPositionElement.classList.add('error_position');
        const errorTypeElement = document.createElement('span');
        errorTypeElement.innerText = error.error_type;
        errorTypeElement.classList.add('error_type');
        const errorMessageElement = parseErrorMessage(error.message);
        errorRowSummaryElem.appendChild(errorPositionElement);
        errorRowSummaryElem.appendChild(document.createTextNode(' '));
        errorRowSummaryElem.appendChild(errorTypeElement);
        errorRowSummaryElem.appendChild(document.createTextNode(': '));
        errorRowSummaryElem.appendChild(errorMessageElement);
        errorRowElem.appendChild(errorRowSummaryElem);
        const errorDetailsElem = document.createElement('div');
        errorDetailsElem.appendChild(errorMessageElement.cloneNode(true));
        errorDetailsElem.classList.add('error_details');
        errorRowElem.appendChild(errorDetailsElem);
        errorElem.appendChild(errorRowElem);
    }
}
export function updateLogELement(displayLogElem, logElem, interpreted) {
    logElem.innerText = interpreted.stdout;
    if (interpreted.stdout.length !== 0) {
        displayLogElem.click();
    }
}
