# oddcli.py - odd command line interpreter

import odd

state = odd.State()

print(state.make_parser("#t #f #t").tree())

#print(state.evaluate("#t"))
