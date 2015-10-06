# odd.py - odd language interpreter in python
import io

class SourceLocation(object):
    "Describes the location of a bit of Odd code, mainly used for helpful error reporting"
    def __init__(self, src, line, column, length):
        self.src = src
        self.line = line
        self.column = column
        self.length = length

# object hierarchy
class Value(object):
    def evaluate(self, state):
        "By default, objects are self-evaluating (this behavior makes sense for numbers, symbols, etc)"
        return self

class Constant(Value):
    def __init__(self, value):
        self.value = value

    def __repr__(self):
        return str(self.value)

TRUE = Constant(True)
FALSE = Constant(False)

class Fixnum(Value):
    def __init__(self, value=0):
        self.value = value

class Symbol(Value):
    def __init__(self, string = ""):
        self.string = string

    def __repr__(self):
        return self.string

class ParserError(Exception):
    def __init__(self, msg, location):
        self.location = location
        self.msg = msg

class Parser(object):
    def __init__(self, src, srcio):
        self.src = src
        if isinstance(srcio, str):
            self.io = io.StringIO(srcio)
        else:
            self.io = srcio
        self.line = 0
        self.col = 0

    def error(self, text, length):
        return ParserError(text, SourceLocation(self.src, self.line, self.col, length))

    def next_token(self):
        while True:
            ch = self.io.read(1)
            self.col += 1
            if ch == '#':
                nch = self.io.read(1)
                if nch == 't':
                    return TRUE
                elif nch =='f':
                    return FALSE
                else:
                    raise self.error("Unknown constant #%s" % nch, 2)
            elif ch == '\n':
                self.line += 1
                self.col = 0
            elif ch == ' ':
                continue
            elif ch == '':
                return None
            else:
                raise self.error("Unknown character '%s'" % ch, 1)

    def next_expr(self):
        token = self.next_token()
        if isinstance(token, Value):
            # Literal
            return token
        pass

    def tree(self):
        exprs = []
        expr = self.next_expr()
        while expr:
            exprs.append(expr)
            expr = self.next_expr()
        return exprs

    @staticmethod
    def parse(state, src, code):
        return Parser(src, code).tree()

class State(object):
    def __init__(self):
        self.symbols = {}
        self.source_files = []

    def register_source(self, path, data): 
        self.source_files.append((path, data,))
        return len(self.source_files) - 1

    def make_symbol(self, string):
        if 'string' in self.symbols:
            return self.symbols[string]
        self.symbols[string] = Symbol(string)
        return self.symbols[string]

    def make_parser(self, code):
        src = self.register_source("python string", code)
        return Parser(src, code)

    def parse(self, value):
        if isinstance(value, str):
            src = self.register_source("python string", value)
            return Parser.parse(self, src, value)
        else:
            raise ValueError("argument %t must be string or file" % value)

    def evaluate(self, value):
        if isinstance(value, Value):
            return value.evaluate(self)
        elif isinstance(value, str):
            return self.parse(value)
        else:
            raise ValueError("argument %r must be string or Odd value" % value)
