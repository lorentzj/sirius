;
export function constraint_name(constraint) {
    if (constraint.data.Eq !== undefined) {
        return `${constraint.data.Eq[0]} == ${constraint.data.Eq[1]}`;
    }
    else {
        return '!!!!';
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
