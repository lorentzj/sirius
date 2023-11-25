import init, * as bindings from '../js/sirius.js';
import * as types from './types.js';

export type InterpretRequest = {
    code: string,
    id: number,
    tokens: types.Token[]
};

export type InterpretResponse = {
    output: types.InterpreterOutput,
    id: number,
    tokens: types.Token[]
};

init().then(_ => {
    let cancel = (v: unknown) => { };

    onmessage = (e: MessageEvent<InterpretRequest>) => {
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

    async function interpret(code: string): Promise<types.InterpreterOutput> {
        return JSON.parse(bindings.interpret(code));
    }
});