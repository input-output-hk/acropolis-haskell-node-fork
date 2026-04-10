def fold_brackets(s):
    depth = 0
    res = ""
    insides = ""
    for symbol in s:
        if symbol in ['{','(','[']:
            depth+=1
            if depth==1: res += symbol

        if symbol in ['}',')',']']:
            depth-=1
            if depth==0:
                res += insides + symbol
                insides = ""

        if depth == 0:
            res += symbol
        else:
            if len(insides) < 1000:
                insides += symbol
            elif len(insides) == 1000:
                insides += '...'
    return res

f = open('log2.steps', 'r')

for ln in f:
    print(fold_brackets(ln))