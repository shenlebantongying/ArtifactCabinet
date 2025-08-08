import numpy as np

l = np.genfromtxt("./data/rosalind_par.txt", skip_header=1, dtype=np.int64)

o = l[0]
r = l[1:]
print(" ".join(map(str, np.concat([r[np.where(r <= o)], l[0:1], r[np.where(r > o)]]))))
