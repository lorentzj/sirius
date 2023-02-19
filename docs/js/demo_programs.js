export async function load_demo_programs() {
    const factorial_src = await fetch('./examples/factorial.srs').then((response) => response.text());
    const tuple_eq_src = await fetch('./examples/tuple_eq.srs').then((response) => response.text());
    const stdlib_src = await fetch('./examples/stdlib.srs').then((response) => response.text());
    const matmul_src = await fetch('./examples/matmul.srs').then((response) => response.text());
    const functional_src = await fetch('./examples/functional.srs').then((response) => response.text());
    return {
        'Factorial': factorial_src,
        'Tuple Equality': tuple_eq_src,
        'Standard Lib': stdlib_src,
        'Matrix Multiply (Aspirational)': matmul_src,
        'Functional (Aspirational)': functional_src,
    };
}
