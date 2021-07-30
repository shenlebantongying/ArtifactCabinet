package main

import (
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"sync"
)

func check(e error, s string) {
	if e != nil {
		println(s)
		panic(e)
	}
}

func exec_inc(i int, wg *sync.WaitGroup) int {
	defer wg.Done()
	output, e := exec.Command("/bin/sh", "-c", "expr 1 + "+strconv.Itoa(i)).Output()
	check(e, "shit")
	ret, _ := strconv.Atoi(strings.TrimSpace(string(output)))
	//fmt.Printf("%v\n", ret)
	return int(ret)
}

func main() {

	wg := new(sync.WaitGroup) // A special counter for Gor

	var slbslice [10]int

	for i := 0; i < 10; i++ {
		wg.Add(1)
		i := i
		go func() {
			slbslice[i] = exec_inc(i, wg)
		}()
	}

	wg.Wait()

	for m, n := range slbslice {
		fmt.Println(m, "->", n)

	}

}
