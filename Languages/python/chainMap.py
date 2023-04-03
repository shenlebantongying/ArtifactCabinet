from collections import ChainMap

a={'a':1,'b':2,'c':3}
b={'d':4,'f':5}

c=ChainMap(a,b)
print(c)

