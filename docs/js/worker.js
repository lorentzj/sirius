import init, * as bindings from '../js/sirius.js';
import * as types from './types.js';
init().then(_ => {
    let cancel = (v) => { };
    onmessage = (e) => {
        cancel(undefined);
        let p = new Promise(resolve => cancel = resolve);
        Promise.race([p, parse(e.data.code, e.data.delayMs)]).then(parsed => {
            if (parsed) {
                postMessage({
                    parsed: parsed,
                    id: e.data.id,
                    andExecute: e.data.andExecute
                });
            }
        });
    };
    async function parse(code, delay) {
        await new Promise(r => setTimeout(r, delay));
        const parser_output = bindings.parse(code);
        return types.prepare_parse_output(parser_output);
    }
});
