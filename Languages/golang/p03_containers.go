package main

import (
	"container/ring"
	"fmt"
	"log"
	"os"
)

var mapper map[int]string

func main() {
	//map functions
	mapper = make(map[int]string)
	files, err := os.ReadDir(".")
	if err != nil {
		log.Fatal(err)
	}
	for x, f := range files {
		mapper[x] = f.Name()
	}

	for i, s := range mapper {
		fmt.Println(i, "->", s)
	}

	//std.ring => infinity data structure

	crystal := ring.New(10)

	for i := 0; i < crystal.Len(); i++ {
		crystal.Value = i * i
		crystal = crystal.Next()
	}

	// Do will loop through everything for one round
	crystal.Do(func(p interface{}) {
		fmt.Println(p.(int))
	})

}
