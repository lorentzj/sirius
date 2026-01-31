export class Spinner {
    constructor(id) {
        const span = document.getElementById(id);
        this.span = span;
    }
    on() {
        this.span.dataset['pendingOps'] = '1';
    }
    off() {
        this.span.dataset['pendingOps'] = '0';
    }
}
