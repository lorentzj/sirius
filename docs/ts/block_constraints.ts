import * as types from './types.js';

function setMouseOverHLHandler(blockConstraintsElem: HTMLElement, editorElem: HTMLElement, blockInfoElem: HTMLElement, blockId: number, blockFirstTokenId: number) {
    blockInfoElem.addEventListener('mouseenter', (_) => {
        const blockInfoSelector = `span.block_info[data-block-id='${blockId}']`;
        const blockInfoElems = blockConstraintsElem.querySelectorAll(blockInfoSelector);
        blockInfoElems.forEach(e => e.classList.add('highlight'));

        const blockFirstTokenSelector = `span.token[data-token-id='${blockFirstTokenId}']`;
        const blockFirstTokenElem = editorElem.querySelector(blockFirstTokenSelector);
        if(blockFirstTokenElem !== null) {
            blockFirstTokenElem.classList.add('highlight');
        }
    });

    blockInfoElem.addEventListener('mouseleave', (_) => {
        const blockInfoSelector = `span.block_info[data-block-id='${blockId}']`;
        const blockInfoElems = blockConstraintsElem.querySelectorAll(blockInfoSelector);
        blockInfoElems.forEach(e => e.classList.remove('highlight'));

        const blockFirstTokenSelector = `span.token[data-token-id='${blockFirstTokenId}']`;
        const blockFirstTokenElem = editorElem.querySelector(blockFirstTokenSelector);
        if(blockFirstTokenElem !== null) {
            blockFirstTokenElem.classList.remove('highlight');
        }
    });
}

type LineBlock = {text: string, block_id: number, first_token: number};

function createConstraintLineDiv(blockConstraintsELem: HTMLElement, editorElem: HTMLElement, lineBlocks: LineBlock[]): HTMLDivElement {
    const lineElem = document.createElement('div');
    if(lineBlocks.length == 0) {
        lineElem.innerText = '\n';
    } else {
        for(const block of lineBlocks) {
            const blockInfoElem = document.createElement('span');
            blockInfoElem.innerText = block.text;
            blockInfoElem.dataset['blockId'] = block.block_id.toString();
            blockInfoElem.classList.add('block_info');
            lineElem.appendChild(blockInfoElem);
            setMouseOverHLHandler(blockConstraintsELem, editorElem, blockInfoElem, block.block_id, block.first_token);
        }
    }
    lineElem.classList.add('block_constraint_line');
    return lineElem;
}

function statementAtLine(statements: types.Statement[], tokens: types.Token[], line: number): types.Statement | null {
    for(const s of statements) {
        if(tokens[s.start].line < line && tokens[s.end].line >= line) {
            return s;
        }
    }

    return null;
}

function constraintLevels(parseOutput: types.ParserOutput): {block_id: number, first_token: number}[][] {
    let functionLines: {start_line: number, end_line: number, first_token: number, function: types.Function}[] = [];

    for(const [_, f] of parseOutput.ast) {
        const firstToken = f.name.start;
        const firstTokenLine =  parseOutput.tokens[firstToken].line + 1;
        const lastToken = f.body.statements[f.body.statements.length - 1].end;
        let lastTokenLine = parseOutput.tokens[lastToken].line;

        functionLines.push({
            start_line: firstTokenLine,
            end_line: lastTokenLine,
            function: f,
            first_token: f.name.start - 1
        })
    }

    functionLines.sort((a, b) => a.start_line - b.start_line);
    
    let currFunction = 0;

    let levels: {block_id: number, first_token: number}[][] = [];

    if(functionLines.length > 0) {
        for(let line = 0; line <= functionLines[functionLines.length - 1].end_line; line++) {
            if(functionLines[currFunction].start_line == line) {
                levels.push([{
                        block_id: functionLines[currFunction].function.id,
                        first_token: functionLines[currFunction].first_token
                }]);
            } else if(functionLines[currFunction].end_line == line - 1) {
                levels.push([]);
                currFunction += 1;
            } else if(levels.length > 0 && levels[levels.length - 1].length != 0) {
                let level = [{
                    block_id: functionLines[currFunction].function.id,
                    first_token: functionLines[currFunction].first_token
                }];
                const functionStatements = functionLines[currFunction].function.body.statements;
                let currStatement = statementAtLine(functionStatements, parseOutput.tokens, line);
                while(currStatement !== null && ('If' in currStatement.data || 'For' in currStatement.data)) {
                    level.push({
                        block_id: currStatement.id,
                        first_token: currStatement.start
                    });
                    if('If' in currStatement.data) {
                        let innerStatements = currStatement.data['If'].true_inner.statements;
                        if(currStatement.data['If'].false_inner !== null) {
                            innerStatements = innerStatements.concat(currStatement.data['If'].false_inner.statements);
                        }
                        currStatement = statementAtLine(innerStatements, parseOutput.tokens, line);
                    } else {
                        let innerStatements = currStatement.data['For'].inner.statements;
                        currStatement = statementAtLine(innerStatements, parseOutput.tokens, line);
                    }
                }

                levels.push(level);
            } else {
                levels.push([]);
            }
        }
    }

    return levels;
}

export function updateBlockConstraintsElem(blockConstraintsElem: HTMLElement, editorElem: HTMLElement, parseOutput: types.ParserOutput) {
    const levels = constraintLevels(parseOutput);
    let res: HTMLElement[] = [];

    const maxDepth = levels.map(v => v.length).reduce((a, b) => a > b ? a : b);

    for(let i = 1; i < levels.length; i++) {
        if(levels[i].length === 0) {
            res.push(createConstraintLineDiv(blockConstraintsElem, editorElem, []));
        } else {
            if(levels[i].length > levels[i - 1].length) {
                let l: LineBlock[] = [];
                if(levels[i - 1].length > 0) {
                    for(let j = 0; j < levels[i - 1].length - 1; ++j) {
                        l.push({text: '┃', ...levels[i][j]});
                    }
                    l.push({text: '┣', ...levels[i][levels[i - 1].length - 1]});
                    for(let j = levels[i - 1].length; j < levels[i].length; ++j) {
                        l.push({text: '┳', ...levels[i][j]});
                    }

                    if(l.length < maxDepth) {
                        l[l.length - 1].text += '━'.repeat(maxDepth - l.length);
                    }
                } else {
                    l.push({text: '┏', ...levels[i][0]});
                    for(let j = 1; j < levels[i].length; ++j) {
                        l.push({text: '┳', ...levels[i][j]});
                    }

                    if(l.length < maxDepth) {
                        l[l.length - 1].text += '━'.repeat(maxDepth - l.length);
                    }
                }
                res.push(createConstraintLineDiv(blockConstraintsElem, editorElem, l));
            } else if(i === levels.length - 1 || levels[i].length > levels[i + 1].length) {
                const diff = i === levels.length - 1 ? levels[i].length : levels[i].length - levels[i + 1].length;
                let l: LineBlock[] = [];
                if(i < levels.length - 1 && levels[i + 1].length > 0) {
                    for(let j = 0; j < levels[i + 1].length - 1; ++j) {
                        l.push({text: '┃', ...levels[i][j]});
                    }
                    l.push({'text': '┣', ...levels[i][levels[i + 1].length - 1]});

                    for(let j = levels[i + 1].length; j < levels[i].length; ++j) {
                        l.push({text: '┻', ...levels[i][j]});
                    }

                    if(l.length < maxDepth) {
                        l[l.length - 1].text += '━'.repeat(maxDepth - l.length);
                    }
                } else {
                    l.push({text: '┗', ...levels[i][0]});
                    for(let j = 1; j < levels[i].length; ++j) {
                        l.push({text: '┻', ...levels[i][j]});
                    }
                    
                    if(l.length < maxDepth) {
                        l[l.length - 1].text += '━'.repeat(maxDepth - l.length);
                    }
                }

                res.push(createConstraintLineDiv(blockConstraintsElem, editorElem, l));
            } else {
                let l:LineBlock[] = [];
                for(let j = 0; j < levels[i].length; ++j) {
                    l.push({text: '┃', ...levels[i][j]});
                }

                res.push(createConstraintLineDiv(blockConstraintsElem, editorElem, l));
            }
        }
    }
    blockConstraintsElem.replaceChildren(...res);
}