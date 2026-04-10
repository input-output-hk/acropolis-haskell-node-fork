class Parser:
    def __init__(self, line):
        self.buf = line
        self.ptr = 0

    def get_lexem(self):
        res = ""
        inside_quote = False
        inside_screen = False

        for p in range(self.ptr, len(self.buf)):
            symbol = self.buf[p]

            if inside_quote:
                if inside_screen:
                    inside_screen = False
                else:
                    if symbol == '\\': inside_screen = True
                    elif symbol == '"': inside_quote = False

            elif symbol == '"':
                inside_quote = True

            res += symbol
            self.ptr = p+1
            if not inside_quote: break

        return res

def removeLists(ln):
    parser = Parser(ln)
    symbol = ""
    nested = 0
    contents = ""
    res = ""
    while (symbol := parser.get_lexem()) != "":
        if symbol == '[': 
            nested += 1
        elif symbol == ']': 
            if nested == 1: res += contents
            nested -= 1
        elif nested > 0:
            contents = "..."
            continue
        else: 
            contents = ""

        res += symbol

    return res

f = open('../log8.86c8a19.a', 'r')
for ln in f:
    print(removeLists(ln))

#idx = ['213','214','215','216','217']
#
#for i in idx:
#    f = open('../log8.epoch%shdr' % i, 'r')
#
#    for ln in f:
#         print(removeLists(ln))
