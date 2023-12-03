;
export function constraint_name(constraint) {
    if ('Eq' in constraint.inner) {
        return `${constraint.inner.Eq[0]} == ${constraint.inner.Eq[1]}`;
    }
    else if ('Neq' in constraint.inner) {
        return `${constraint.inner.Neq[0]} != ${constraint.inner.Neq[1]}`;
    }
    else if ('Gt' in constraint.inner) {
        return `${constraint.inner.Gt[0]} > ${constraint.inner.Gt[1]}`;
    }
    else if ('GtEq' in constraint.inner) {
        return `${constraint.inner.GtEq[0]} >= ${constraint.inner.GtEq[1]}`;
    }
    else {
        return '[Internal Error]';
    }
}
export function prepare_parse_output(parse_output) {
    const parsed = JSON.parse(parse_output);
    parsed.typeTokens = new Set(parsed.typeTokens);
    let hl_map = new Map();
    Object.keys(parsed.highlightMap).forEach(key => {
        hl_map.set(Number.parseInt(key), parsed.highlightMap[key]);
    });
    parsed.highlightMap = hl_map;
    let ast = new Map();
    Object.keys(parsed.ast).forEach(key => {
        ast.set(key, parsed.ast[key]);
    });
    parsed.ast = ast;
    return parsed;
}
