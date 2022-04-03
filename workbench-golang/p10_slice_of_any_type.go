package main

import "fmt"

func sum_all_int(s []interface{}) int {
	var acc int = 0
	for _, a := range s {
		switch a.(type) {
		case int:
			acc = acc + a.(int)
		default:
		}
	}
	return acc
}

func main() {
	var myslice = []interface{}{1, "a", 3}
	fmt.Println(sum_all_int(myslice))
}
