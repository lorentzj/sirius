demo:
    wasm-pack build --target web --out-dir docs/public/js
    mv docs/public/js/sirius.js docs/ts/sirius/wasm
    mv docs/public/js/sirius.d.ts docs/ts/sirius/wasm
    mv docs/public/js/sirius_bg.wasm docs/ts/sirius/wasm
    rm -rf docs/public/js
    cd docs && npm run build

test:
    cargo test