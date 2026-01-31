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
    line.textContent = `[Ln ${error.start_line}] `;

    const type = document.createElement('span');
    type.classList.add('error_type');
    type.textContent = `${error.type}Error`;
    summary.appendChild(line);
    summary.appendChild(type);
    details.appendChild(summary);
    details.appendChild(document.createTextNode(error.message));
    details.setAttribute('data-line', error.start_line.toString());

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