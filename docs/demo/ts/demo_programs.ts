export async function load_demo_programs(): Promise<{[name: string]: string}> {
    const pythag_source = await fetch('./examples/pythag.hsp').then((response) => response.text());
    const types_source = await fetch('./examples/types.hsp').then((response) => response.text());
    const if_source = await fetch('./examples/if.hsp').then((response) => response.text());
    return {
        'Pythagoras': pythag_source,
        'Types':      types_source,
        'If':         if_source
    }
}