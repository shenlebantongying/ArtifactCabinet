package main

import "fmt"

func SumIntsOrFloats[K comparable, V int64 | float64](m map[K]V) V{
	var s V
	for _, v := range m {
		s += v
	}
	return s
}

func main(){
	
	ints := map[string]int64{
		"first": 34,
		"second": 12,
	}

	floats := map[string]float64{
		"one": 34,
		"two": 12,
	}

	fmt.Printf("%v",
				SumIntsOrFloats(ints))
}