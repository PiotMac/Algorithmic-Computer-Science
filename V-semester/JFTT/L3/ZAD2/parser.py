from sly import Parser
from lexer import CalcLexer

GF = 1234577
rpn_string = ""

def reset():
    global rpn_string
    rpn_string = ""

def extended_gcd(a, b):
    if a == 0:
        return (b, 0, 1)
    else:
        g, x, y = extended_gcd(b % a, a)
        return (g, y - (b // a) * x, x)

def mod_inverse(a, m):
    g, x, y = extended_gcd(a, m)
    if g != 1:
        return -1
    else:
        return x % m

def invert(num, m):
    return mod_inverse(num, m)

class CalcParser(Parser):
    # Get the token list from the lexer (required)
    tokens = CalcLexer.tokens
    #debugfile = 'parser.out'

    precedence = (
        ('left', 'ADD', 'SUB'),
        ('left', 'MUL', 'DIV', 'MOD'),
        ('right', 'NEG'),
        ('nonassoc', 'POW')
    )

    # Grammar rules and actions
    @_('expr RESULT')
    def line(self, p):
        global rpn_string
        print(rpn_string)
        print("Wynik: " f"{p[0]}")
        reset()

    @_('')
    def line(self, p):
        reset()

    @_('error RESULT')
    def line(self, p):
        print("Błąd składni!")
        reset()

    @_('NUM')
    def expr(self, p):
        global rpn_string
        rpn_string += f"{p[0] % GF} "
        return p[0] % GF

    @_('SUB NUM %prec NEG')
    def expr(self, p):
        global rpn_string
        rpn_string += f"{(-p[1] % GF + GF) % GF} "
        return (-p[1] % GF + GF) % GF

    @_('expr ADD expr')
    def expr(self, p):
        global rpn_string
        rpn_string += "+ "
        return (p[0] + p[2]) % GF

    @_('expr SUB expr')
    def expr(self, p):
        global rpn_string
        rpn_string += "- "
        return (p[0] - p[2]) % GF

    @_('expr MUL expr')
    def expr(self, p):
        global rpn_string
        rpn_string += "* "
        return (p[0] * p[2]) % GF

    @_('expr DIV expr')
    def expr(self, p):
        if p[2] == 0:
            while True:
                tok = next(self.tokens, None)
                if not tok or tok.type == 'RESULT':
                    break
            print(f"Błąd: dzielenie przez zero")
            reset()
            raise Exception
        else:
            global rpn_string
            rpn_string += "/ "
            result = invert(p[2], GF)
            if result == -1:
                while True:
                    tok = next(self.tokens, None)
                    if not tok or tok.type == 'RESULT':
                        break
                print(f"Błąd: {p[2]} nie jest odwracalne modulo {GF}")
                while True:
                    tok = next(self.tokens, None)
                    if not tok or tok.type == 'RESULT':
                        break
                reset()
                raise Exception
            else:
                result = result % GF
                return (p[0] * result) % GF

    @_('expr POW expr1')
    def expr(self, p):
        global rpn_string
        rpn_string += "^ "
        output = 1
        for i in range(0, p[2]):
            output *= p[0]
            output = ((output % GF) + GF) % GF
        return output

    @_('expr MOD expr')
    def expr(self, p):
        if p[2] == 0:
            while True:
                tok = next(self.tokens, None)
                if not tok or tok.type == 'RESULT':
                    break
            print(f"Błąd: modulo przez zero")
            reset()
            raise Exception
        else:
            global rpn_string
            rpn_string += "% "
            return (p[0] % p[2]) % GF

    @_('LPAR expr RPAR')
    def expr(self, p):
        return p[1]

    @_('SUB LPAR expr RPAR %prec NEG')
    def expr(self, p):
        global rpn_string
        rpn_string += "~ "
        return ((-p[2] % GF) + GF) % GF

    @_('NUM')
    def expr1(self, p):
        global rpn_string
        rpn_string += f"{p[0] % (GF - 1)} "
        return p[0] % (GF - 1)

    @_('SUB NUM %prec NEG')
    def expr1(self, p):
        global rpn_string
        rpn_string += f"{(-p[1] % (GF - 1) + (GF - 1)) % (GF - 1)} "
        return (-p[1] % (GF - 1) + (GF - 1)) % (GF - 1)

    @_('expr1 ADD expr1')
    def expr1(self, p):
        global rpn_string
        rpn_string += "+ "
        return (p[0] + p[2]) % (GF - 1)

    @_('expr1 SUB expr1')
    def expr1(self, p):
        global rpn_string
        rpn_string += "- "
        return (p[0] - p[2]) % (GF - 1)

    @_('expr1 MUL expr1')
    def expr1(self, p):
        global rpn_string
        rpn_string += "* "
        return (p[0] * p[2]) % (GF - 1)

    @_('expr1 DIV expr1')
    def expr1(self, p):
        if p[2] == 0:
            while True:
                tok = next(self.tokens, None)
                if not tok or tok.type == 'RESULT':
                    break
            print(f"Błąd: dzielenie przez zero")
            reset()
            raise Exception
        else:
            global rpn_string
            rpn_string += "/ "
            result = invert(p[2], GF - 1)
            if result == -1:
                print(f"Błąd: {p[2]} nie jest odwracalne modulo {GF - 1}")
                while True:
                    tok = next(self.tokens, None)
                    if not tok or tok.type == 'RESULT':
                        break
                reset()
                raise Exception
            else:
                result = result % (GF - 1)
                return (p[0] * result) % (GF - 1)

    @_('expr1 MOD expr1')
    def expr1(self, p):
        if p[2] == 0:
            while True:
                tok = next(self.tokens, None)
                if not tok or tok.type == 'RESULT':
                    break
            print(f"Błąd: modulo przez zero")
            reset()
            raise Exception
        else:
            global rpn_string
            rpn_string += "% "
            return (p[0] % p[2]) % (GF - 1)

    @_('LPAR expr1 RPAR')
    def expr1(self, p):
        return p[1]

    @_('SUB LPAR expr1 RPAR %prec NEG')
    def expr1(self, p):
        global rpn_string
        rpn_string += "~ "
        return ((-p[2] % (GF - 1)) + (GF - 1)) % (GF - 1)

    def error(self, p):
        while True:
            tok = next(self.tokens, None)
            if not tok or tok.type == 'RESULT':
                break
        print("Błąd składni!")
        self.restart()
        reset()
