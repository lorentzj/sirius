function errorItem(error) {
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
    return details;
}
export class ErrorList {
    constructor(id) {
        this.setErrors = (errors) => {
            this.div.innerHTML = '';
            for (const error of errors) {
                this.div.appendChild(errorItem(error));
            }
        };
        this.div = document.getElementById(id);
    }
}
