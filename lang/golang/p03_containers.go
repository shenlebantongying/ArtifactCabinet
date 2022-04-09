package main

//TODO: check if there are some good community containers

import (
	"container/ring"
	"fmt"
	"io/ioutil"
	"log"
)

var mapper map[int]string

func main() {
	//map functions
	mapper = make(map[int]string)
	files, err := ioutil.ReadDir(".")
	if err != nil {
		log.Fatal(err)
	}
	for x, f := range files {
		mapper[x] = f.Name()
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
