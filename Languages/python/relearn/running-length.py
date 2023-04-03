def run_length_encoding(si:str):
    return rec(si,0,si[0])

def rec(s,n,cur):
    if len(s) == 0:
        return str(n)+cur
    elif s[0] == cur:
        return rec(s[1:],n+1,cur)
    else:
        return str(n)+cur+rec(s[1:],1,s[0])
            
print(run_length_encoding("aaabbabbbbbbbbbbb"))
print(run_length_encoding("abbbbbbbaaaaaaaaabbbbbbbbbaaaaaaaaa"))
