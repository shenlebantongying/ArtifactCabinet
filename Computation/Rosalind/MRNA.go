//go:build ignore

package main

import (
	"fmt"
	"math/big"
	"os"
	"strings"
)

func check(err error) {
	if err != nil {
		os.Exit(1)
	}

}

func main() {
	data, err := os.ReadFile("./data/rosalind_mrna.txt")
	check(err)
	codon_table, err := os.ReadFile("./codon_table_RNA.txt")
	check(err)
	codon_str := string(codon_table)

	rna_to_dna := make(map[string]big.Int)

	one := big.NewInt(1)

	for i, c := range strings.Fields(codon_str) {
		if i%2 == 1 {
			b, _ := rna_to_dna[c]
			rna_to_dna[c] = *b.Add(&b, one)
		}
	}

	fmt.Println(rna_to_dna)

	n := big.NewInt(1)
	for _, c := range data {

		b, ok := rna_to_dna[string(c)]
		if ok {
			n.Mul(n, &b)
		}
	}

	b, ok := rna_to_dna["Stop"]
	if !ok {
		os.Exit(1)
	}

	n.Mul(n, &b)
	n.Mod(n, big.NewInt(1000000))

	fmt.Println(n)
}
