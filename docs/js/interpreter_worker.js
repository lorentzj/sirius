import init, * as bindings from '../js/sirius.js';
init().then(_ => {
    let cancel = (v) => { };
    onmessage = (e) => {
        cancel(undefined);
        let p = new Promise(resolve => cancel = resolve);
        Promise.race([p, interpret(e.data.code)]).then(output => {
            if (output) {
                postMessage({
                    output: output,
                    id: e.data.id,
                    tokens: e.data.tokens
                });
            }
        });
    };
    async function interpret(code) {
        return JSON.parse(bindings.interpret(code));
    }
});
