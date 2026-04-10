import re

f = open('log7.618.split','r')
res = {}
removed = {}
for ln in f:
    g = re.match(r'.*reward for.*R=Coin (\d+).*', ln)
    if g:
        res[g.group(1)] = 1

f = open('log7.618.cmp','r')
for ln in f:
    g = re.match(r' \\xe([0-9a-f]+) .* \| *(\d+)', ln)
    if g:
       if g.group(2) not in res:
           print('Not found: ', g.group(1), g.group(2))
       elif g.group(2) in removed:
           print('Duplicate: ',g.group(1), g.group(2))
       else:
           print('Found: ', g.group(1), g.group(2))
           removed[g.group(2)] = 1

print('**** cmp2 ****')

removed = {}

f = open('log7.618.cmp2','r')
for ln in f:
    g = re.match(r'h=\"([0-9a-f]+)\" to_pay=(\d+)', ln)
    if g:
       if g.group(2) not in res:
           print('Not found: ', g.group(1), g.group(2))
       elif g.group(2) in removed:
           print('Duplicate: ',g.group(1), g.group(2))
       else:
           print('Found: ', g.group(1), g.group(2))
           removed[g.group(2)] = 1

print(res.keys())