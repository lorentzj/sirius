import * as types from './types.js';

function updateToolTip(tooltipElem: HTMLElement, token: HTMLElement) {
    tooltipElem.innerHTML = '';
    if(token.dataset['type'] !== undefined) {
        let typeElem = document.createElement('span');
        typeElem.classList.add('code');
        typeElem.classList.add('token');
        typeElem.classList.add('type');        
        typeElem.innerText = token.dataset['type'];

        tooltipElem.appendChild(typeElem);
    }
}

export function setHandler(tooltipElem: HTMLElement, tokenElem: HTMLElement, tokenId: number, ast: types.AST) {
    let type = getIdentType(tokenId, ast);

    if(type !== null) {
        tokenElem.dataset['type'] = type;
        tokenElem.addEventListener('mouseenter', () => {
            updateToolTip(tooltipElem, tokenElem);
            tooltipElem.style.top  =  `${tokenElem.getBoundingClientRect().bottom}px`;
            tooltipElem.style.left =  `${tokenElem.getBoundingClientRect().left}px`;
            tooltipElem.style.display = 'block';
        });
        tokenElem.addEventListener('mouseleave', () => {
            tooltipElem.style.display = 'none';
        });
    }
}

function getIdentType(tokenId: number, ast: types.AST): string | null {
    for(let f of ast.values()) {
        let foundInFn = getIdentTypeInBlock(tokenId, f.body);
        if(foundInFn !== null) {
            return foundInFn;
        }
    }

    return null;
}

function getIdentTypeInBlock(tokenId: number, statements: types.Statement[]): string | null {
    for(let statement of statements) {
        if(statement.start <= tokenId && statement.end > tokenId) {
            if('Let' in statement.data) {
                let foundInExpr = getIdentTypeInExpr(tokenId, statement.data['Let'][2]);
                if(foundInExpr !== null) {
                    return foundInExpr;
                }
            } else if('Assign' in statement.data) {
                let foundInExpr = getIdentTypeInExpr(tokenId, statement.data['Assign'][1]);
                if(foundInExpr !== null) {
                    return foundInExpr;
                }
            } else if('Print' in statement.data) {
                let foundInExpr = getIdentTypeInExpr(tokenId, statement.data['Print']);
                if(foundInExpr !== null) {
                    return foundInExpr;
                }
            } else if('Return' in statement.data && statement.data['Return'] !== null) {
                let foundInExpr = getIdentTypeInExpr(tokenId, statement.data['Return']);
                if(foundInExpr !== null) {
                    return foundInExpr;
                }
            } else if('If' in statement.data) {
                let foundInExpr = getIdentTypeInExpr(tokenId, statement.data['If'][0]);
                if(foundInExpr !== null) {
                    return foundInExpr;
                }

                let foundInTrueBlock = getIdentTypeInBlock(tokenId, statement.data['If'][1]);
                if(foundInTrueBlock !== null) {
                    return foundInTrueBlock;
                }

                if(statement.data['If'][2] !== null) {
                    let foundInFalseBlock = getIdentTypeInBlock(tokenId, statement.data['If'][2]);
                    if(foundInFalseBlock !== null) {
                        return foundInFalseBlock;
                    }    
                }                
            } else if('For' in statement.data) {
                let foundInFrom = getIdentTypeInExpr(tokenId, statement.data['For'][1]);
                if(foundInFrom !== null) {
                    return foundInFrom;
                }

                let foundInTo = getIdentTypeInExpr(tokenId, statement.data['For'][2]);
                if(foundInTo !== null) {
                    return foundInTo;
                }

                let foundInInnerBlock = getIdentTypeInBlock(tokenId, statement.data['For'][3]);
                if(foundInInnerBlock !== null) {
                    return foundInInnerBlock;
                }
            }
            return null;
        }
    }
    return null;
}

function getIdentTypeInExpr(tokenId: number, expression: types.Expression): string | null {
    if(expression.start <= tokenId && expression.end > tokenId) {
        if('F64' in expression.data) {
            return expression.t;
        } else if('I64' in expression.data) {
            return expression.t;
        } else if('Bool' in expression.data) {
            return expression.t;
        } else if('Ident' in expression.data) {
            return expression.t;
        } else if('BinaryOp' in expression.data) {
            let lhs = expression.data['BinaryOp'][0];
            let rhs = expression.data['BinaryOp'][2];

            if(lhs.start <= tokenId && lhs.end > tokenId) {
                return getIdentTypeInExpr(tokenId, lhs);
            } else if(rhs.start <= tokenId && rhs.end > tokenId) {
                return getIdentTypeInExpr(tokenId, rhs);
            }
        } else if('UnaryOp' in expression.data) {
            let inner = expression.data['UnaryOp'][1];
            if(inner.start <= tokenId && inner.end > tokenId) {
                return getIdentTypeInExpr(tokenId, inner);
            }
        } else if('Tuple' in expression.data) {
            for(let inner of expression.data['Tuple']) {
                if(inner.start <= tokenId && inner.end > tokenId) {
                    return getIdentTypeInExpr(tokenId, inner);
                }    
            }
        } else if('FnCall' in expression.data) {
            let caller = expression.data['FnCall'][0];
            if(caller.start <= tokenId && caller.end > tokenId) {
                return getIdentTypeInExpr(tokenId, caller);
            }

            for(let inner of expression.data['FnCall'][1]) {
                if(inner.start <= tokenId && inner.end > tokenId) {
                    return getIdentTypeInExpr(tokenId, inner);
                }    
            }
        } else if('Accessor' in expression.data) {
            let lhs = expression.data['Accessor'][0];
            let rhs = expression.data['Accessor'][1];

            if(lhs.start <= tokenId && lhs.end > tokenId) {
                return getIdentTypeInExpr(tokenId, lhs);
            } else if(rhs.start <= tokenId && rhs.end > tokenId) {
                return getIdentTypeInExpr(tokenId, rhs);
            }
        }
    }
    
    return null;
}