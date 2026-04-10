def fold_brackets(s):
    depth = 0
    res = ""
    for symbol in s:
        if (symbol == '[') or ((depth > 0) and (symbol in ['[','(','{'])):
            depth+=1

        if (symbol == ']') or ((depth > 0) and (symbol in [']',')','}'])):
            depth-=1

        if (symbol in [',','[']) and (depth == 1):
            res += symbol + "\n    "
        else:
            res += symbol

    return res

f = open('../log8.c2f104187be.214', 'r')

for ln in f:
    print(fold_brackets(ln))