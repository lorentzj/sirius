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
    {Error: string};

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

export type S = 
    { 'Let': [Positioned<string>, Positioned<string> | null, Expression] }
    | { 'Assign': [Positioned<string>, Expression] }
    | { 'Print': Expression }
    | { 'Return': Expression | null }
    | { 'If': [Expression, Statement[], Statement[] | null]}
    | { 'For': [Positioned<string>, Expression, Expression, Statement[]]};

export type Statement = Positioned<S>;

export type Constraint = Positioned<{
    Eq: [string, string]
}>;

export type Function = {
    name: string
    type_args: Positioned<string>[],
    args: [Positioned<string>, Positioned<string>][],
    return_type: Positioned<string>,
    body: Statement[],
    constraints: Constraint[]
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
        return '!!!!';
    }
}