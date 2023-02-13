export async function load_demo_programs(): Promise<{[name: string]: string}> {
    const pythag_src    = await fetch('./examples/pythag.srs').then((response) => response.text());
    const factorial_src = await fetch('./examples/factorial.srs').then((response) => response.text());
    const tuple_eq_src  = await fetch('./examples/tuple_eq.srs').then((response) => response.text());
    const externals_src = await fetch('./examples/externals.srs').then((response) => response.text());

    return {
        'Pythagoras':     pythag_src,
        'Factorial':      factorial_src,
        'Tuple Equality': tuple_eq_src,
        'Externals':      externals_src,
    }
}