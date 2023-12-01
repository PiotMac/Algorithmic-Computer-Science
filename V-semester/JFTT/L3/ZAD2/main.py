from lexer import CalcLexer
from parser import CalcParser


def main():
    lexer = CalcLexer()
    parser = CalcParser()
    while True:
        text = ""
        try:
            while True:
                text += input()
                text += '\n'
                if not text.endswith('\\\n'):
                    break
            parser.parse(lexer.tokenize(text))
        except Exception as e:
            # Reset the whole thing
            parser.restart()


if __name__ == "__main__":
    main()