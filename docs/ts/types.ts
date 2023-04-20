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
}

export type ParserOutput = {
    tokens: Token[],
    errors: Error[],
    typeTokens: Set<number>,
    highlightMap: Map<number, number[]>
};

export type InterpreterOutput = {
    stdout: string,
    error: Error | null
}