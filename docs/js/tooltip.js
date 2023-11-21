import { parseErrorMessage } from './output_area.js';
function updateToolTip(tooltipElem, tokenElem) {
    tooltipElem.innerHTML = '';
    let tooltipInner = document.createElement('div');
    tooltipInner.id = 'tooltip_inner';
    tooltipElem.appendChild(tooltipInner);
    if (tokenElem.dataset['type'] !== undefined) {
        let typeElem = document.createElement('span');
        typeElem.classList.add('code');
        typeElem.classList.add('token');
        typeElem.classList.add('type');
        typeElem.innerText = tokenElem.dataset['type'];
        tooltipInner.appendChild(typeElem);
    }
    if (tokenElem.dataset['error'] !== undefined) {
        const errorTypeElem = document.createElement('span');
        errorTypeElem.innerText = tokenElem.dataset['errorType'] + ': ';
        errorTypeElem.classList.add('error_type');
        let errorDetailsElem = parseErrorMessage(tokenElem.dataset['error']);
        let errorMsgElem = document.createElement('div');
        errorMsgElem.appendChild(errorTypeElem);
        errorMsgElem.appendChild(errorDetailsElem);
        tooltipInner.appendChild(errorMsgElem);
    }
}
export function addTypeInfo(editorElem, ast) {
    for (let [token, type] of getIdentTypePairs(ast)) {
        const tokenSelector = `span.token[data-token-id='${token}']`;
        const tokenElem = editorElem.querySelector(tokenSelector);
        if (tokenElem !== null) {
            tokenElem.dataset['type'] = type;
        }
    }
}
export function setToolTipHandler(tooltipElem, tokenElem) {
    tokenElem.addEventListener('mouseenter', () => {
        if (tokenElem.dataset['error'] !== undefined || tokenElem.dataset['type'] !== undefined) {
            updateToolTip(tooltipElem, tokenElem);
            tooltipElem.style.top = `${tokenElem.getBoundingClientRect().bottom + window.scrollY}px`;
            tooltipElem.style.left = `${tokenElem.getBoundingClientRect().left + window.scrollX}px`;
            tokenElem.appendChild(tooltipElem);
            tooltipElem.style.display = 'block';
        }
    });
    tokenElem.addEventListener('mouseleave', () => {
        tooltipElem.style.display = 'none';
    });
}
function getIdentTypePairs(ast) {
    let r = [];
    for (let f of ast.values()) {
        r = r.concat(getIdentTypePairsInBlock(f.body.statements));
    }
    return r;
}
function getIdentTypePairsInBlock(statements) {
    let r = [];
    for (let statement of statements) {
        if ('Let' in statement.data) {
            r.push([statement.data['Let'].name.start, statement.data['Let'].bound_type]);
            r = r.concat(getIdentTypePairsInExpr(statement.data['Let'].value));
        }
        else if ('Assign' in statement.data) {
            r = r.concat(getIdentTypePairsInExpr(statement.data['Assign'].value));
        }
        else if ('Print' in statement.data) {
            r = r.concat(getIdentTypePairsInExpr(statement.data['Print']));
        }
        else if ('Return' in statement.data && statement.data['Return'] !== null) {
            r = r.concat(getIdentTypePairsInExpr(statement.data['Return']));
        }
        else if ('If' in statement.data) {
            r = r.concat(getIdentTypePairsInExpr(statement.data['If'].condition));
            r = r.concat(getIdentTypePairsInBlock(statement.data['If'].true_inner.statements));
            if (statement.data['If'].false_inner !== null) {
                r = r.concat(getIdentTypePairsInBlock(statement.data['If'].false_inner.statements));
            }
        }
        else if ('For' in statement.data) {
            r.push([statement.data['For'].iterator.start, statement.data['For'].iterator_type]);
            r = r.concat(getIdentTypePairsInExpr(statement.data['For'].from));
            r = r.concat(getIdentTypePairsInExpr(statement.data['For'].to));
            r = r.concat(getIdentTypePairsInBlock(statement.data['For'].inner.statements));
        }
    }
    return r;
}
function getIdentTypePairsInExpr(expression) {
    if ('Ident' in expression.data) {
        return [[expression.start, expression.t]];
    }
    else if ('BinaryOp' in expression.data) {
        let lhs = expression.data['BinaryOp'].lhs;
        let rhs = expression.data['BinaryOp'].rhs;
        return getIdentTypePairsInExpr(lhs).concat(getIdentTypePairsInExpr(rhs));
    }
    else if ('UnaryOp' in expression.data) {
        let inner = expression.data['UnaryOp'].inner;
        return getIdentTypePairsInExpr(inner);
    }
    else if ('Tuple' in expression.data) {
        let r = [];
        for (let inner of expression.data['Tuple']) {
            r = r.concat(getIdentTypePairsInExpr(inner));
        }
        return r;
    }
    else if ('FnCall' in expression.data) {
        let caller = expression.data['FnCall'].func;
        let r = [];
        r = r.concat(getIdentTypePairsInExpr(caller));
        for (let inner of expression.data['FnCall'].args) {
            r = r.concat(getIdentTypePairsInExpr(inner));
        }
        return r;
    }
    else if ('Accessor' in expression.data) {
        let lhs = expression.data['Accessor'].target;
        let rhs = expression.data['Accessor'].index;
        return getIdentTypePairsInExpr(lhs).concat(getIdentTypePairsInExpr(rhs));
    }
    else {
        return [];
    }
}
