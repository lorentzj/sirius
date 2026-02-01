export var TokenType;
(function (TokenType) {
    TokenType["KEYWORD"] = "KEYWORD";
    TokenType["NUMBER"] = "NUMBER";
    TokenType["IDENTIFIER"] = "IDENTIFIER";
    TokenType["OPERATOR"] = "OPERATOR";
})(TokenType || (TokenType = {}));
export class Token {
    constructor(line, column) {
        this.type = TokenType.IDENTIFIER;
        this.value = '';
        this.start = { line, column };
        this.end = { line, column };
    }
    setType() {
        if (/^let$/.test(this.value)) {
            this.type = TokenType.KEYWORD;
        }
        else if (/^\+|\=|\*$/.test(this.value)) {
            this.type = TokenType.OPERATOR;
        }
        else if (/^\d+(\.\d+)?$/.test(this.value)) {
            this.type = TokenType.NUMBER;
        }
        else {
            this.type = TokenType.IDENTIFIER;
        }
    }
}
export function tokenize(lines) {
    const tokens = [];
    for (let line = 0; line < lines.length; line++) {
        const lineCode = lines[line];
        let currentToken = new Token(line, 0);
        for (let charIndex = 0; charIndex < lineCode.length; charIndex++) {
            const char = lineCode[charIndex];
            if (/^\s$/.test(char)) {
                if (currentToken.value !== '') {
                    currentToken.setType();
                    tokens.push(currentToken);
                    currentToken = new Token(line, charIndex + 1);
                }
                continue;
            }
            currentToken.value += char;
            currentToken.end.column += 1;
        }
        if (currentToken.value !== '') {
            currentToken.setType();
            tokens.push(currentToken);
        }
    }
    return tokens;
}
