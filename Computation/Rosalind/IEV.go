//go:build ignore

package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
)

func main() {
	data, _ := os.ReadFile("./data/rosalind_iev.txt")
	factor := [...]float64{2, 2, 2, 2 * (0.5 + 0.5*0.5), 2 * 0.5, 0}

	var sum float64 = 0
	for i, s := range strings.Fields(string(data)) {
		v, _ := strconv.ParseFloat(s, 64)
		sum += v * factor[i]
	}

	fmt.Printf("%.1f\n", sum)

}
