export interface Error {
    error_type: string,
    message: string,
    tokens: number[]
};

export type TokenType = 
    {Op: string} |
    {Constant: number} |
    {Identifier: string} |
    {Keyword: string};

export interface Token {
    token_type: string | TokenType,
    line: number,
    start: number,
    end: number
}

export type ParserOutput = {
    tokens: Token[],
    errors: Error[],
    bracket_pairs: [number, number][],
    type_tokens: number[];
};

export type InterpreterOutput = {
    output: string,
    error: Error | null
}