import init, * as bindings from '../js/sirius.js';
import * as editor from './editor.js';
import * as types from './types.js';

function generateSectionFootnotes(section: HTMLElement, section_id: number) {
    let ss = 1;
    let footnotes = [];
    let sups = section.getElementsByTagName('sup');
    for(const sup of sups) {
        if(sup.classList.contains('external')) {
            const link = sup.firstChild as HTMLAnchorElement;
            link.innerText = '[🡥]';
            link.target = '_blank';
        } else if(sup.classList.contains('footnote')) {
            const link = document.createElement('a');
            link.classList.add('footnote_backlink');
            const backlink_id = `footnote_backlink_${section_id}_${ss}`;
            const id = `footnote_${section_id}_${ss}`;
            
            sup.prepend(document.createTextNode(`${ss}. `));
            
            const footnote_html = sup.innerHTML;

            link.href = `#${id}`;
            link.id = backlink_id;

            link.innerText = `[${ss}]`;
            
            sup.innerHTML = '';
            sup.appendChild(link);

            footnotes.push([footnote_html, id, backlink_id]);

            ss += 1;
        }
    }

    if(footnotes.length > 0) {
        let footnoteContainer = document.createElement('div');
        footnoteContainer.classList.add('footnote_container');

        for(let [footnote, id, backlink_id] of footnotes) {
            let footnoteElem = document.createElement('div');
            footnoteElem.id = id;
            footnoteElem.classList.add('footnote_content');
            footnoteElem.innerHTML = footnote;

            const link = document.createElement('a');
            link.href = `#${backlink_id}`;
            link.innerText = '[back]';
            footnoteElem.appendChild(document.createTextNode(' '));
            footnoteElem.appendChild(link);
            footnoteContainer.appendChild(footnoteElem);
        }    

        section.appendChild(footnoteContainer);
    }
}

document.body.onload = () => {
    let sections = document.getElementsByTagName('section');

    for(const [section_id, section] of [...sections].entries()) {
        generateSectionFootnotes(section, section_id);
        const header = section.firstElementChild;

        if(header !== null && header.tagName == 'H2') {
            const anchor = document.createElement('a');
            anchor.innerText = '#';
            anchor.href = `#${section.id}`;
            anchor.title = 'Permalink';
            header.prepend(document.createTextNode(' '));
            header.prepend(anchor);
        }
    }

    init().then(() => {
        let tooltipElem = document.getElementById('tooltip') as HTMLElement;

        for(const [section_id, section] of [...sections].entries()) {
            let blocksToParse = section.querySelectorAll('code.block.please_parse');
            
            for(const block of blocksToParse) {
                (block as HTMLElement).style.display = 'flex';
                const code = (block as HTMLElement).innerText;
                const parser_output = types.prepare_parse_output(bindings.parse(code));

                const codeElem = document.createElement('div');
                codeElem.classList.add('parsed_code_block', 'column');
                const blockLinesElem = document.createElement('div');
                blockLinesElem.classList.add('line_numbers', 'column');

                block.innerHTML = '';
                block.appendChild(blockLinesElem);
                block.appendChild(codeElem);

                editor.updateEditorWithCode(codeElem, tooltipElem, blockLinesElem, code.split('\n'), parser_output);
                editor.updateEditorWithErrors(parser_output.errors, codeElem);
            }
        }
    });
}