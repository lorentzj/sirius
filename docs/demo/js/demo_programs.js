export async function load_demo_programs() {
    const pythag_source = await fetch('./examples/pythag.srs').then((response) => response.text());
    return {
        'Pythagoras': pythag_source,
    };
}
