import { Editor } from './editor.js';
import { ErrorList } from './error_list.js';
import init, { lex } from './sirius/sirius.js';

function main() {
    const worker = new Worker('./js/worker.js', {'type': 'module'});
    
    init().then(() => {
        const editor = new Editor('editor', 'compiler_spinner', lex, worker);
        const errorList = new ErrorList('errors');
        editor.addErrorListener(errorList.setErrors);
    });
}

if (document.readyState === 'complete') {
    main();
} else {
    document.addEventListener('DOMContentLoaded', main);
}