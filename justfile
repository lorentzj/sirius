docs:
    cargo doc --no-deps --target-dir docs/public/api
    wasm-pack build --target web --out-dir docs/public/js
    mv docs/public/js/sirius.js docs/ts/sirius/wasm
    mv docs/public/js/sirius.d.ts docs/ts/sirius/wasm
    mv docs/public/js/sirius_bg.wasm docs/ts/sirius/wasm
    rm -rf docs/public/js
    cd docs && npm run build
    mkdir docs/public/js/z3
    cp docs/node_modules/z3-solver/build/z3-built.js docs/public/js/z3
    cp docs/node_modules/z3-solver/build/z3-built.wasm docs/public/js/z3

test:
    cargo test