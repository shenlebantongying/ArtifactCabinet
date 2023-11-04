package main

import (
	"fmt"
	"math/rand"
)

func main() {
	const N = 60
	const INDEX = 4
	// original will be [1:4]
	var original = [N]int{}
	var result = [N]int{}

	var count = [INDEX + 1]int{0}

	for i := 0; i < N; i++ {
		original[i] = rand.Intn(INDEX)
	}

	fmt.Println(original)

	// Sorting
	for i := 0; i < N; i++ {
		count[original[i]+1]++
	}

	for i := 0; i < INDEX; i++ {
		count[i+1] += count[i]
	}

	for i := 0; i < N; i++ {
		result[count[original[i]]] = original[i]
		count[original[i]] += 1
	}

	fmt.Println(result)

}
