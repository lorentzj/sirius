import init, { compile } from './sirius/sirius.js';
import { WorkerRequest } from './editor.js';
import { debounce } from './utils.js';

init().then(() => {
    onmessage = debounce((e: MessageEvent<WorkerRequest>) => {
        const output = compile(e.data.code);
        postMessage({
            output,
            editId: e.data.editId
        });
    });

    postMessage({ ready: true });
});