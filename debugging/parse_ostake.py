import re

f = open('log4.ostake','r')
less = 0
equal = 0
greater = 0

for ln in f:
    g = re.match('.*unKeyHash = \"([0-9a-f]+)\".*plege=(\d+).*ostake=(\d+).*', ln)
    if g:
        key = g.group(1)
        plege = int(g.group(2))
        ostake = int(g.group(3))
        if plege < ostake:
            less += 1
        elif plege == ostake:
            equal += 1
        else:
            greater += 1
            print('key = %s, plege = %d > ostake = %d' % (key,plege,ostake))

print('Stats: |plege<ostake|=%d, |plege=ostake|=%d, |plege>ostake|=%d' % (less,equal,greater))