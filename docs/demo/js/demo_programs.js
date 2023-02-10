export async function load_demo_programs() {
    const pythag_source = await fetch('./examples/pythag.srs').then((response) => response.text());
    const types_source = await fetch('./examples/types.srs').then((response) => response.text());
    const scopes_source = await fetch('./examples/scopes.srs').then((response) => response.text());
    return {
        'Pythagoras': pythag_source,
        'Types': types_source,
        'Scopes': scopes_source
    };
}
