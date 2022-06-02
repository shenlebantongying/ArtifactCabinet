n_const = 3

def is_prime_aux(a):
    if a==1 :
        return True
    elif n_const % a == 0:
        return False
    else:
        return(is_prime_aux(a-1))
        
print(is_prime_aux(n_const-1))
