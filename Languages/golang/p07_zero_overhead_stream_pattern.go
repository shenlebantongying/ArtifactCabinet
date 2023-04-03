package main

import (
	"fmt"
)

// Channel is cheap but not free

// Does this pattern is in use because Go lack map/reduce/filter to process unlimited length data?

func stream(cc func(int)) {
	for _, x := range []int{1, 2, 3} {
		cc(x)
	}
}

func stream2(cc func(int), l []int) {
	for _, x := range l {
		cc(x)
	}
}

func main() {
	stream(func(x int) { fmt.Println(x + 1) })

	fmt.Println("------")

	stream2(
		func(x int) { x = x + 1; fmt.Println(x) },
		[]int{1, 2, 3})
	// Note that this slice can be arbitrary length

}
