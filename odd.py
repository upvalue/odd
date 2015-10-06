# odd.py - odd language interpreter in python
import io, string

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
        if self.value == True:
            return "#t"
        elif self.value == False:
            return "#f"

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

class Expression(Value):
    def __init__(self, tipe, exprs):
        self.tipe = tipe 
        self.exprs = exprs

class Parser(object):
    RESERVED_PUNCTUATION = ".#"
    SYMBOL_CHARS = string.ascii_letters + string.punctuation

    def __init__(self, state, src, srcio):
        self.state = state
        self.src = src
        if isinstance(srcio, str):
            self.io = io.BytesIO(srcio.encode('utf-8'))
        else:
            self.io = srcio
        self.line = 0
        self.col = 0

    def error(self, text, length):
        return ParserError(text, SourceLocation(self.src, self.line, self.col, length))

    def next_char(self):
        return self.io.read(1).decode('utf-8')

    def peek_char(self):
        ch = self.next_char()
        self.io.seek(-1, 1)
        return ch

    def next_symbol(self, ch):
        string = ch
        while True:
            ch = self.io.read(1).decode('utf-8')
            self.col += 1
            if (ch in Parser.SYMBOL_CHARS) and (ch not in Parser.RESERVED_PUNCTUATION) and ch != '':
                string += ch
            else:
                return self.state.make_symbol(string)

    def newline(self):
        self.line += 1
        self.col = 0

    def eat_comment(self):
        while True:
            ch = self.next_char()
            if ch == '\n':
                self.newline()
                return
            elif ch == '':
                return

    def next_token(self):
        while True:
            ch = self.next_char()
            self.col += 1
            if ch == '#':
                nch = self.next_char()
                if nch == 't':
                    return TRUE
                elif nch =='f':
                    return FALSE
                else:
                    raise self.error("Unknown constant #%s" % nch, 2)
            elif ch == '\n':
                self.newline()
            elif ch == ' ':
                continue
            elif ch == '/':
                tst = self.peek_char()
                if tst == '/':
                    self.eat_comment()
                else:
                    return self.next_symbol(ch)
            elif ch != '' and (ch in Parser.SYMBOL_CHARS) and (ch not in Parser.RESERVED_PUNCTUATION):
                return self.next_symbol(ch)
            elif ch == '':
                return None
            else:
                raise self.error("Unknown character '%s'" % ch, 1)

    def next_expr(self):
        token = self.next_token()
        # Literal
        if isinstance(token, Value):
            return token
        pass

    def exprs(self):
        exprs = []
        expr = self.next_expr()
        while expr:
            exprs.append(expr)
            expr = self.next_expr()
        return exprs

    @staticmethod
    def parse(state, src, code):
        return Parser(state, src, code).exprs()

class State(object):
    primitives = {}

    def __init__(self):
        self.symbols = {}
        self.source_files = []

    def register_source(self, path, data): 
        self.source_files.append((path, data,))
        return len(self.source_files) - 1

    def make_symbol(self, string):
        if string in self.symbols:
            return self.symbols[string]
        self.symbols[string] = Symbol(string)
        return self.symbols[string]

    def make_parser(self, code):
        src = self.register_source("python string", code)
        return Parser(self, src, code)

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
            return [expr.evaluate(self) for expr in self.parse(value)]
        else:
            raise ValueError("argument %r must be string or Odd value" % value)

def primitive(name=None):
    def decorator(function):
        nonlocal name
        if name == None:
            name = function.__name__
        State.primitives[name] = function
        return function
    return decorator

@primitive
def identity(state, arg):
    return arg

if __name__ == "__main__":
    state = State()

    print(state.parse("hello #t #f"))
    print(state.evaluate("hello #t #f"))
    #print(state.make_parser("#t #f #t").exprs())

