import { init, Z3LowLevel } from 'z3-solver/build/browser';

const STATUS = 0, SIZE = 1;
const IDLE = 0, CLAIMED = 1, REQUEST = 2, OK = 3, ERR = 4, OVERFLOW = 5;

let z3: null | Z3LowLevel["Z3"] = null;
let busy = false;

export type Channel = {
    state: Int32Array;
    buf: Uint8Array;
}

export function channel(bufSize: number): Channel {
    return {
        state: new Int32Array(new SharedArrayBuffer(4 * 2)),
        buf: new Uint8Array(new SharedArrayBuffer(bufSize))
    }
}

export function runSync(channel: Channel, smt2: string, wakeZ3Worker: () => void, timeoutMS = 30_000): string {
    const inBytes = (new TextEncoder()).encode(smt2);
    if (inBytes.length > channel.buf.length) {
        throw new Error(`z3 query too large: ${inBytes.length} > ${channel.buf.length}`);
    }

    const prev = Atomics.compareExchange(channel.state, STATUS, IDLE, CLAIMED);
    if (prev !== IDLE) {
        throw new Error(`z3 channel should be idle (found ${prev})`);
    }

    channel.buf.set(inBytes, 0);
    Atomics.store(channel.state, SIZE, inBytes.length);
    Atomics.store(channel.state, STATUS, REQUEST);
    wakeZ3Worker();

    const until = Date.now() + timeoutMS;
    let status: number;
    for (;;) {
        status = Atomics.load(channel.state, STATUS);
        if (status !== REQUEST) break;
        const left = until - Date.now();
        if (left <= 0) {
            Atomics.store(channel.state, STATUS, IDLE);
            throw new Error('solver timed out');
        }
        Atomics.wait(channel.state, STATUS, REQUEST, left);
    }

    const size = Atomics.load(channel.state, SIZE);
    const payload = new TextDecoder().decode(channel.buf.slice(0, size));
    Atomics.store(channel.state, STATUS, IDLE);

    switch (status) {
        case OK: return payload;
        case ERR: throw new Error(payload);
        case OVERFLOW: throw new Error(`z3 response too large: ${size} bytes`);
        default: throw new Error(`bad state ${status}`);
    }
}

export async function serve(channel: Channel) {
    if(busy || Atomics.load(channel.state, STATUS) !== REQUEST) {
        return;
    }

    busy = true;

    let status = ERR;
    let out = new Uint8Array(0);

    try {
        if (z3 === null) {
            z3 = (await init()).Z3;
        }

        const size = Atomics.load(channel.state, SIZE);
        const smt2 = (new TextDecoder()).decode(channel.buf.slice(0, size));

        const cfg = z3.mk_config();
        const ctx = z3.mk_context(cfg);
        z3.del_config(cfg);
        try {
            const response = await z3.eval_smtlib2_string(ctx, smt2);
            out = (new TextEncoder()).encode(response);
            status = OK;
        } finally {
            z3.del_context(ctx);
        }
    } catch (e) {
        out = (new TextEncoder()).encode(String(e));
        status = ERR;
    }

    if (status === OK && out.length > channel.buf.length) {
        Atomics.store(channel.state, SIZE, out.length);
        status = OVERFLOW;
    } else {
        channel.buf.set(out, 0);
        Atomics.store(channel.state, SIZE, out.length);
    }

    Atomics.store(channel.state, STATUS, status);
    Atomics.notify(channel.state, STATUS);
    busy = false;
}