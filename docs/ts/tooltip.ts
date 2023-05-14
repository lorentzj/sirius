import * as types from './types.js';
import {parseErrorMessage} from './output_area.js';

function updateToolTip(tooltipElem: HTMLElement, tokenElem: HTMLElement) {
    tooltipElem.innerHTML = '';
    if(tokenElem.dataset['type'] !== undefined) {
        let typeElem = document.createElement('span');
        typeElem.classList.add('code');
        typeElem.classList.add('token');
        typeElem.classList.add('type');        
        typeElem.innerText = tokenElem.dataset['type'];

        tooltipElem.appendChild(typeElem);
    }

    if(tokenElem.dataset['error'] !== undefined) {
        const errorTypeElem = document.createElement('span');
        errorTypeElem.innerText = tokenElem.dataset['errorType'] + ': ';
        errorTypeElem.classList.add('error_type');

        let errorDetailsElem = parseErrorMessage(tokenElem.dataset['error']);

        let errorMsgElem = document.createElement('div');

        errorMsgElem.appendChild(errorTypeElem);
        errorMsgElem.appendChild(errorDetailsElem);
        tooltipElem.appendChild(errorMsgElem);
    }
}

export function addTypeInfo(editorElem: HTMLElement, ast: types.AST) {
    for(let [token, type] of getIdentTypePairs(ast)) {
        const tokenSelector = `span.token[data-token-id='${token}']`;
        const tokenElem = editorElem.querySelector(tokenSelector);
        if(tokenElem !== null) {
            (tokenElem as HTMLElement).dataset['type'] = type;
        }
    }
}

export function setToolTipHandler(tooltipElem: HTMLElement, tokenElem: HTMLElement) {
    tokenElem.addEventListener('mouseenter', () => {
        if(tokenElem.dataset['error'] !== undefined || tokenElem.dataset['type'] !== undefined) {
            updateToolTip(tooltipElem, tokenElem);
            tooltipElem.style.top  =  `${tokenElem.getBoundingClientRect().bottom + window.scrollY}px`;
            tooltipElem.style.left =  `${tokenElem.getBoundingClientRect().left + window.scrollX}px`;
            tokenElem.appendChild(tooltipElem);
            tooltipElem.style.display = 'block';    
        }
    });

    tokenElem.addEventListener('mouseleave', () => {
        tooltipElem.style.display = 'none';
    });
}

function getIdentTypePairs(ast: types.AST): [number, string][] {
    let r: [number, string][] = [];
    for(let f of ast.values()) {
        r = r.concat(getIdentTypePairsInBlock(f.body[0]));
    }

    return r;
}

function getIdentTypePairsInBlock(statements: types.Statement[]): [number, string][] {
    let r: [number, string][] = [];
    for(let statement of statements) {
        if('Let' in statement.data) {
            r.push([statement.data['Let'][0].start, statement.data['Let'][3]]);
            r = r.concat(getIdentTypePairsInExpr(statement.data['Let'][4]));
        } else if('Assign' in statement.data) {
            r = r.concat(getIdentTypePairsInExpr(statement.data['Assign'][1]));
        } else if('Print' in statement.data) {
            r = r.concat(getIdentTypePairsInExpr(statement.data['Print']));
        } else if('Return' in statement.data && statement.data['Return'] !== null) {
            r = r.concat(getIdentTypePairsInExpr(statement.data['Return']));
        } else if('If' in statement.data) {
            r = r.concat(getIdentTypePairsInExpr(statement.data['If'][0]));
            r = r.concat(getIdentTypePairsInBlock(statement.data['If'][2][0]));
            if(statement.data['If'][3] !== null) {
                r = r.concat(getIdentTypePairsInBlock(statement.data['If'][3][0]));
            }
        } else if('For' in statement.data) {
            r.push([statement.data['For'][0].start, statement.data['For'][1]]);
            r = r.concat(getIdentTypePairsInExpr(statement.data['For'][3]));
            r = r.concat(getIdentTypePairsInExpr(statement.data['For'][4]));
            r = r.concat(getIdentTypePairsInBlock(statement.data['For'][5][0]));
        }
    }

    return r;
}

function getIdentTypePairsInExpr(expression: types.Expression): [number, string][] {
    if('Ident' in expression.data) {
        return [[expression.start, expression.t]];
    } else if('BinaryOp' in expression.data) {
        let lhs = expression.data['BinaryOp'][0];
        let rhs = expression.data['BinaryOp'][2];

        return getIdentTypePairsInExpr(lhs).concat(getIdentTypePairsInExpr(rhs));

    } else if('UnaryOp' in expression.data) {
        let inner = expression.data['UnaryOp'][1];
        return getIdentTypePairsInExpr(inner);
    } else if('Tuple' in expression.data) {
        let r: [number, string][] = [];

        for(let inner of expression.data['Tuple']) {
            r = r.concat(getIdentTypePairsInExpr(inner));
        }
        return r;
    } else if('FnCall' in expression.data) {
        let caller = expression.data['FnCall'][0];

        let r: [number, string][] = [];
        r = r.concat(getIdentTypePairsInExpr(caller));

        for(let inner of expression.data['FnCall'][1]) {
            r = r.concat(getIdentTypePairsInExpr(inner));
        }
        return r;
    } else if('Accessor' in expression.data) {
        let lhs = expression.data['Accessor'][0];
        let rhs = expression.data['Accessor'][1];

        return getIdentTypePairsInExpr(lhs).concat(getIdentTypePairsInExpr(rhs));
    } else {
        return [];
    }
}