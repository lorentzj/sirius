export class Spinner {
    span: HTMLSpanElement

    constructor(id: string) {
        const span = document.getElementById(id)! as HTMLSpanElement;
        this.span = span;
    }

    on() {
        this.span.dataset['pendingOps'] = '1';    
    }

    off() {
        this.span.dataset['pendingOps'] = '0';    
    }
}