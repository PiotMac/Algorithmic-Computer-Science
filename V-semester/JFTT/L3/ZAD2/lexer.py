from sly import Lexer

GF = 1234577
class CalcLexer(Lexer):
    # Set of token names
    tokens = { NUM, ADD, SUB, MUL ,DIV, POW, MOD, LPAR, RPAR, RESULT, ERR}

    ignore_comment = r'^\#(\\\n|.)*\n'
    ignore_linecontinuation = r'\\\n'

    # String containing ignored characters between tokens
    ignore = ' \t'

    # Regular expression rules for tokens
    @_(r'\d+')
    def NUM(self, t):
        t.value = int(t.value)
        return t

    ADD = r'\+'
    SUB = r'\-'
    MUL = r'\*'
    DIV = r'\/'
    POW = r'\^'
    MOD = r'\%'
    LPAR = r'\('
    RPAR = r'\)'
    RESULT = r'\n'
    ERR = r'.'

    def error(self, t):
        pass


if __name__ == '__main__':
    data = '#fas\\\n65\nafsa\n'
    lexer = CalcLexer()
    for tok in lexer.tokenize(data):
        print(tok)