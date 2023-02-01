export async function load_demo_programs(): Promise<{[name: string]: string}> {
    const pythag_source = await fetch('./examples/pythag.hsp').then((response) => response.text());
    return {
        'Pythagoras': pythag_source
    }
}