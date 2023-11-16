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
    | { 'Ident': [string, Positioned<string>[] | null]}
    | { 'BinaryOp': [Expression, Op, Expression]}
    | { 'UnaryOp': ['ArithNeg' | 'BoolNeg' | 'Tick', Expression]}
    | { 'Tuple': Expression[] }
    | { 'Accessor': [Expression, Expression] }
    | { 'FnCall': [Expression, Expression[]] };

export type Expression = {
    start: number,
    data: E,
    t: string,
    end: number
};

export type Block = [Statement[], Constraint[]];

export type S = 
    { 'Let': [Positioned<string>, boolean, Positioned<string> | null, string, Expression] }
    | { 'Assign': [Positioned<string>, Expression] }
    | { 'Print': Expression }
    | { 'Return': Expression | null }
    | { 'If': [Expression, Constraint[], Block, Block | null]}
    | { 'For': [Positioned<string>, string, Constraint[], Expression, Expression, Block]};

export type Statement = Positioned<S>;

export type Constraint = Positioned<{
    Eq: [string, string]
}>;

export type Function = {
    name: Positioned<string>,
    type_args: Positioned<string>[],
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
    if(constraint.data.Eq !== undefined) {
        return `${constraint.data.Eq[0]} == ${constraint.data.Eq[1]}`;
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