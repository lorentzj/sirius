export interface Error {
    error_type: string,
    message: string,
    start: number,
    end: number
};

export type Tok = 
    string |
    {Op: string} |
    {Float: number} |
    {Int: number} |
    {Identifier: string} |
    {Keyword: string} |
    {Error: string} |
    {IndentError: string};

export type Token = {
    data: Tok,
    line: number,
    start: number,
    end: number
};

export type Positioned<T> = {
    start: number,
    data: T,
    end: number
};

export type Op = 
    'Dot' 
    | 'Exp'
    | 'Tick'
    | 'Mul'
    | 'Div'
    | 'Add'
    | 'Sub'
    | 'And'
    | 'Or'
    | 'Greater'
    | 'Less'
    | 'Equal'
    | 'NotEqual';

export type E = 
    { 'F64': number }
    | { 'I64': string | null }
    | { 'Bool': boolean }
    | { 'Ident': { 'name': string, 'type_args': Positioned<string>[] | null }}
    | { 'BinaryOp': { 'lhs': Expression, 'op': Op, 'rhs': Expression }}
    | { 'UnaryOp': { 'op': 'ArithNeg' | 'BoolNeg' | 'Tick', 'inner': Expression }}
    | { 'Tuple': Expression[] }
    | { 'Accessor': { target: Expression, index: Expression } }
    | { 'FnCall': { func: Expression, args: Expression[] } };

export type Expression = {
    start: number,
    data: E,
    t: string,
    end: number
};

export type Block = {
    statements: Statement[],
    post_constraints: Constraint[]
};

export type S = 
    { 'Let': { 
        name: Positioned<string>,
        mutable: boolean,
        annotation: Positioned<string> | null,
        bound_type: string,
        value: Expression }
    }
    | { 'Assign': { 'place': Positioned<string>, 'value': Expression } }
    | { 'Print': Expression }
    | { 'Return': Expression | null }
    | { 'If': { 
        'condition': Expression,
        'pre_constraints': Constraint[],
        'true_inner': Block,
        'false_inner': Block | null }
    }
    | { 'For': {
        'iterator': Positioned<string>,
        'iterator_type': string,
        'pre_constraints': Constraint[],
        'from': Expression,
        'to': Expression,
        'inner': Block
    }
};

export type Statement = Positioned<S>;

export type Constraint = Positioned<
    { Eq: [string, string] } |
    { Neq: [string, string] } |
    { Gt: [string, string] } |
    { GtEq: [string, string] } 
>;

export type Function = {
    name: Positioned<string>,
    type_args: Positioned<string>[],
    ind_args: Positioned<string>[],
    args: [Positioned<string>, Positioned<string>][],
    return_type: Positioned<string>,
    body: Block
};

export type AST = Map<string, Function>;

export type ParserOutput = {
    tokens: Token[],
    errors: Error[],
    typeTokens: Set<number>,
    highlightMap: Map<number, number[]>,
    ast: AST
};

export type InterpreterOutput = {
    stdout: string,
    error: Error | null
};

export function constraint_name(constraint: Constraint): string {
    if('Eq' in constraint.data) {
        return `${constraint.data.Eq[0]} == ${constraint.data.Eq[1]}`;
    } else if('Neq' in constraint.data) {
        return `${constraint.data.Neq[0]} != ${constraint.data.Neq[1]}`;
    } else if('Gt' in constraint.data) {
        return `${constraint.data.Gt[0]} > ${constraint.data.Gt[1]}`;
    } else if('GtEq' in constraint.data) {
        return `${constraint.data.GtEq[0]} >= ${constraint.data.GtEq[1]}`;
    } else {
        return '[Internal Error]';
    }
}

export function prepare_parse_output(parse_output: string): ParserOutput {
    const parsed = JSON.parse(parse_output) as ParserOutput;
    parsed.typeTokens = new Set(parsed.typeTokens as unknown as number[]);
    
    let hl_map: Map<number, number[]> = new Map();
    Object.keys(parsed.highlightMap as unknown as {[k: string]: number[]}).forEach(key => {
        hl_map.set(Number.parseInt(key), (parsed.highlightMap as unknown as {[k: string]: number[]})[key]);
    });
    parsed.highlightMap = hl_map;
    
    let ast: Map<string, Function> = new Map();
    Object.keys(parsed.ast as unknown as {[k: string]: Function}).forEach(key => {
        ast.set(key, (parsed.ast as unknown as {[k: string]: Function})[key]);
    });
    parsed.ast = ast;    

    return parsed;
}