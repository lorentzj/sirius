import init, { compile } from './sirius/sirius.js';
import { WorkerRequest } from './editor.js';

let bounceMs = 50;

function debounce(callback: (e: MessageEvent<WorkerRequest>) => void) {
    let timeoutId: number | null = null;
    return (e: MessageEvent<WorkerRequest>) => {
        if(timeoutId !== null) {
            clearTimeout(timeoutId);
        }
        timeoutId = setTimeout(() => {
            callback(e);
        }, bounceMs);
    };
}

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