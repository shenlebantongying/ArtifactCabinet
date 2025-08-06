# Bioinformatics, Rosalind Franklin

+ <https://rosalind.info/problems/tree-view/>
+ <https://rosalind.info/users/shenlebantongying/>

# Backgrounds

## IPRB, Mendelian Inheritance

<https://www.genome.gov/genetics-glossary/homozygous>

- allele -> dominant and recessive
- homozygous
- heterozygous

## FIB

<https://en.wikipedia.org/wiki/Fibonacci_sequence>

$$
F(N) = F(N-1) + k * F(N-2)
$$

TODO: solve this visually

## FIBD, fib with deaths

One proof,
+ https://arxiv.org/abs/2312.13098v3
+ https://bica.the-ica.org/Volumes/103//Reprints/BICA2024-01-Reprint.pdf

Insane
+ https://www.fq.math.ca/list-of-issues.html

## PROT, Genetic code, Codon


## TREE, find graph components

TODO: graph has many representations, try something else

TODO: boost's graph library?

TODO: combined with probability?

- https://networkx.org
- https://graph-tool.skewed.de/
- https://igraph.org/

## SPLC,

- intron
- amino acid

## LIA, binomial theorem

Because all mating individuals including the initial one are `AaBb`. The probability for an individual to be a particular alleles is unchanged across generations.

In $k$ generation, there will be $2^k$ individuals.

$$
\begin{align*}
    P[\text{AaBb}] &= 1/4 \\
    P[\text{Not AaBb}] &= 3/4
\end{align*}
$$

The probability of k generation having at least $N$ `AaBb` is thus

$$
    \sum_{i=N}^{2^k} \binom{2^k}{i} \times P[\text{AaBb}]^i  (1-P[\text{AaBb}])^{2^k-i}
$$

TODO: review binomial

## LCSM, longest common subsequence

TODO: read the complexity analysis again...

## MAJ, major elements

TODO: O(1) method?

## MPRT, http get..

Note that subsequences finding problems in bioinformatics generally wants to all subsequences including the ones that overlapps.

Regex needs trick like `(?=(N[^P][ST][^P]))`. Though, this is no longer regular expression anymore?