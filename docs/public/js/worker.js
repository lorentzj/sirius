import init, { compile } from './sirius/sirius.js';
let bounceMs = 50;
function debounce(callback) {
    let timeoutId = null;
    return (e) => {
        if (timeoutId !== null) {
            clearTimeout(timeoutId);
        }
        timeoutId = setTimeout(() => {
            callback(e);
        }, bounceMs);
    };
}
init().then(() => {
    onmessage = debounce((e) => {
        const output = compile(e.data.code);
        postMessage({
            output,
            editId: e.data.editId
        });
    });
    postMessage({ ready: true });
});
