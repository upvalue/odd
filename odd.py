# odd.py - odd language interpreter in python

# object hierarchy
class Value(object):
    pass

class Fixnum(object):
    def __init__(self, value=0):
        self.value = value

class Symbol(Value):
    def __init__(self, string = ""):
        self.string = string

    def __repr__(self):
        return self.string

class Parser(object):
    def __init__(self):
        pass

class State(object):
    def __init__(self):
        self.symbols = {}

    def make_symbol(self, string):
        if 'string' in self.symbols:
            return self.symbols[string]
        self.symbols[string] = Symbol(string)
        return self.symbols[string]

