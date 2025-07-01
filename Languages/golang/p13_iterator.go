package main

import "fmt"

type SignedNum interface {
	~int | ~int8 | ~int16 | ~int32 | ~int64
}

func CloseRange[V SignedNum](start V, end V) func(func(V) bool) {
	return func(yield func(V) bool) {
		for i := start; i <= end; i++ {
			if !yield(i + 1) {
				return
			}
		}

	}
}

func main() {

	myIter := CloseRange[int64](5, 10)
	for x := range myIter {
		fmt.Println(x)
	}

}
