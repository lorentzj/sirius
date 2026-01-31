demo:
    wasm-pack build --target web --out-dir docs/public/js/sirius
    rm docs/public/js/sirius/.gitignore
    mv docs/public/js/sirius/sirius.d.ts docs/ts/sirius/sirius.d.ts
    tsc -p docs/

test:
    cargo test