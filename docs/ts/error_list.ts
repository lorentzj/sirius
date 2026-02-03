export interface Error {
    type: string;
    message: string;
    start_line: number;
    start_column: number;
    end_line: number;
    end_column: number;
}

function errorItem(error: Error) {
    const details = document.createElement('details');
    const summary = document.createElement('summary');
    const line = document.createElement('span');
    line.textContent = `[Ln ${error.start_line + 1}] `;

    const type = document.createElement('span');
    type.classList.add('error_type');
    type.textContent = `${error.type}Error`;
    summary.appendChild(line);
    summary.appendChild(type);
    summary.appendChild(document.createTextNode(': '));
    summary.appendChild(parseErrorMessage(error.message));

    details.appendChild(summary);
    details.appendChild(parseErrorMessage(error.message));

    return details;
}

export class ErrorList {
    div: HTMLDivElement

    constructor(id: string) {
        this.div = document.getElementById(id)! as HTMLDivElement;
    }

    setErrors = (errors: Error[]) => {
        this.div.innerHTML = '';
        for(const error of errors) {
            this.div.appendChild(errorItem(error));
        }
    }
}

export function parseErrorMessage(errorMessage: string): HTMLElement {
    const messageElement = document.createElement('span');
    const codePattern = /"(.*?)"/sg;
    let match = codePattern.exec(errorMessage);
    let i = 0;

    while(match !== null) {
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