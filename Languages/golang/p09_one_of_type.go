package main

import (
	"fmt"
)

// an Atom can be either a string or int

func pprint09(a interface{}) {
	switch a.(type) {
	case int:
		fmt.Printf("int -> %d\n", a)
	case string:
		fmt.Printf("str -> %v\n", a)
	}
}

func main() {
	pprint09("a")
	pprint09(123)
}
