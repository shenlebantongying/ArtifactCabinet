package main

import "fmt"

//Variadic functions!

// Tail recursion
// TODO: check if go support this usage
func recSum(car int, cdr ...int) int {
	if len(cdr) == 0 {
		return car
	} else {
		return car + recSum(cdr[0], cdr[1:len(cdr)]...)
	}
}

// Parameter passing in go.
func sumPtr(a int, b int) int {
	return b + a
}

func sumRef(a *int, b *int) int {
	return *b + *a
}

// modify a reference type;
func inc(a *int) {
	*a += 1
}

func main() {
	var aray = [2]int{1, 2}

	inc(&aray[0])

	fmt.Println("ptr sum:", sumPtr(1, aray[1]))
	fmt.Println("ref sum:", sumRef(&aray[0], &aray[1]))
	fmt.Println(recSum(1, 2, 3, 4, 5))

	// quirks => .toString() in java
	fmt.Printf("Get type in go: %T\n", aray)
}
