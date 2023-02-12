export async function load_demo_programs() {
    const pythag_source = await fetch('./examples/pythag.srs').then((response) => response.text());
    const factorial_source = await fetch('./examples/factorial.srs').then((response) => response.text());
    const tuple_eq_source = await fetch('./examples/tuple_eq.srs').then((response) => response.text());
    return {
        'Pythagoras': pythag_source,
        'Factorial': factorial_source,
        'Tuple Equality': tuple_eq_source,
    };
}
