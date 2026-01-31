import init from './sirius/sirius.js';
init().then(() => {
    onmessage = (e) => {
        postMessage({
            parsed: 123,
            editId: e.data.editID
        });
    };
});
