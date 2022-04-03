package main

import (
	"fmt"
)

// an Atom can be either a string or int

func pprint(a interface{}) {
	switch a.(type) {
	case int:
		fmt.Printf("int -> %d\n", a)
	case string:
		fmt.Printf("str -> %v\n", a)
	}
}

func main() {
	pprint("a")
	pprint(123)
}
