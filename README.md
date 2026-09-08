# Sirius

The WIP prototype compiler for `Sirius`: a small, imperative, total, polynomially bounded language for pointful array programming. Learn more in the [intro](https://www.sirius-lang.org/intro).

## Quick Start

`Sirius` expects the [Z3 CLI](https://z3prover-z3.mintlify.app/installation) to be installed. The typechecker is exposed with the binary `srschk`:

```
$ z3 --version
$ git clone https://github.com/lorentzj/sirius.git
$ cd sirius
$ cargo build
$ cargo test
$ echo "
fn dot{N}(a: f32[N], b: f32[N]) -> f32:
    let mut sum = 0.0
    for i from 0 to N:
        sum += a[i] * b[i]
    return sum

fn matmul{I, J, K}(a: f32[I, J], b: f32[J, K]) -> f32[I, K]:
    for i from 0 to I:
        for k from 0 to K:
            yield dot(a[i], b'[k])" > test.srs
$ target/debug/srschk test.srs
```

## Build the Docs

After `npm install`ing the dependencies in `docs/`, the `just docs` recipe will build the docs in `docs/public`. The recipe uses [wasm-pack](https://wasm-bindgen.github.io/wasm-pack/installer/). 

The live editor requires [cross-origin isolated](https://web.dev/articles/coop-coep) headers (since it uses `SharedMemoryArray`), which are a hassle to set up locally. `npm run serve` uses [miniserve](https://github.com/svenstaro/miniserve) and expects a TLS certificate at `~/mkcert`.