package main

import "fmt"

func sumAllNums(s []interface{}) float32 {
	var acc float32 = 0
	for _, a := range s {
		switch a.(type) {
		case int:
			acc = acc + float32(a.(int))
		case float32:
			acc = acc + a.(float32)
		default:
		}
	}
	return acc
}

func main() {
	var myslice = []interface{}{1, "a", 3, float32(1.1)}
	fmt.Println(sumAllNums(myslice))
}
