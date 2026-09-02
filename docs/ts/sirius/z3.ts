import { init, Z3LowLevel } from 'z3-solver/build/browser';

let z3: null | Z3LowLevel["Z3"] = null;

export type Channel = {
    state: Int32Array;
    buf: Uint8Array;
}
export function channel(bufSize: number): Channel {
    // [message_size, pending_flag]
    return {
        state: new Int32Array(
            new SharedArrayBuffer(Int32Array.BYTES_PER_ELEMENT * 2),
        ),
        buf: new Uint8Array(
            new SharedArrayBuffer(bufSize),
        )
    }
}

export function runSync(channel: Channel, smt2: string): string {
    const inBytes = (new TextEncoder()).encode(smt2);
    if (inBytes.length > channel.buf.length) {
        throw new Error(`message too big: ${inBytes.length} > ${channel.buf.length}`);
    }

    // wait until there's no pending job (pending_flag = 0)
    Atomics.wait(channel.state, 1, 1);

    // store script
    channel.buf.set(inBytes, 0);    
    Atomics.store(channel.state, 0, inBytes.length);

    // submit job
    Atomics.store(channel.state, 1, 1);

    // wait until there's no pending job
    Atomics.wait(channel.state, 1, 1);

    // get response
    const size = Atomics.load(channel.state, 0);
    const outBytes = channel.buf.slice(0, size);

    return (new TextDecoder()).decode(outBytes);    
}

export async function processWhenAny(channel: Channel, pollMS: number) {
    if(z3 === null) {
        let { Z3 } = await init();
        z3 = Z3;
    }

    // if there's a pending job (pending_flag = 1)
    if(Atomics.load(channel.state, 1) == 1) {
        // get script
        const size = Atomics.load(channel.state, 0);
        const inBytes = channel.buf.slice(0, size);
        const smt2 = (new TextDecoder()).decode(inBytes);

        // run Z3
        const cfg = z3.mk_config();
        const ctx = z3.mk_context(cfg);
        z3.del_config(cfg);

        const response = await z3.eval_smtlib2_string(ctx, smt2);
        z3.del_context(ctx);

        const outBytes = (new TextEncoder()).encode(response);

        if (outBytes.length > channel.buf.length) {
            throw new Error(`message too big: ${outBytes.length} > ${channel.buf.length}`);
        }

        // store response
        channel.buf.set(outBytes, 0);    
        Atomics.store(channel.state, 0, outBytes.length);

        // all done (pending_flag = 0)
        Atomics.store(channel.state, 1, 0);
        Atomics.notify(channel.state, 1);
    }
    
    setTimeout(() => processWhenAny(channel, pollMS), pollMS);
}