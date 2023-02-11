export async function load_demo_programs(): Promise<{[name: string]: string}> {
    const pythag_source = await fetch('./examples/pythag.srs').then((response) => response.text());
    const factorial_source = await fetch('./examples/factorial.srs').then((response) => response.text());

    return {
        'Pythagoras': pythag_source,
        'Factorial': factorial_source,
    }
}