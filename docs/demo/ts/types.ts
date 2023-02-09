export interface Error {
    error_type: string,
    message: string,
    start: number,
    end: number
};

export type Tok = 
    string |
    {Op: string} |
    {Constant: number} |
    {Identifier: string} |
    {Keyword: string} |
    {Error: string};

export type Token = {
    data: Tok,
    line: number,
    start: number,
    end: number
}

export type ParserOutput = {
    tokens: Token[],
    errors: Error[],
    type_tokens: Set<number>,
    ast: any
};

export type InterpreterOutput = {
    output: string,
    error: Error | null
}