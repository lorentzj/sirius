import init, * as bindings from '../js/sirius.js';
import * as types from './types.js';

export type ParseRequest = {
    code: string,
    id: number,
    delayMs: number,
    andExecute: boolean
};

export type ParseResponse = {
    parsed: types.ParserOutput,
    id: number,
    andExecute: boolean
};

init().then(_ => {
    let cancel = (v: unknown) => {};

    onmessage = (e: MessageEvent<ParseRequest>) => {
        cancel(undefined);
        let p = new Promise(resolve => cancel = resolve);
        Promise.race([p, parse(e.data.code, e.data.delayMs)]).then(parsed => {
            if(parsed) {
                postMessage({
                    parsed: parsed,
                    id: e.data.id,
                    andExecute: e.data.andExecute
                });
            }
        });
    };

    async function parse(code: string, delay: number): Promise<types.ParserOutput> {
        let minWait = new Promise(r => setTimeout(r, delay));
        const parser_output = bindings.parse(code);
        const parser_output_prepped = types.prepare_parse_output(parser_output);

        await minWait;
        return parser_output_prepped;
    }
});