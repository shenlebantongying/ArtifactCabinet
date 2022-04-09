package main

import (
	"math/rand"
	"sync"
	"time"
)

//TODO: compare Goroutines with threads

func main() {
	rand.Seed(time.Now().UTC().UnixNano())

	wg := new(sync.WaitGroup) // A special counter for Gor
	c := make(chan int)

	for i := 0; i < 10; i++ {
		wg.Add(1)
		go goRand(c, wg)
	}

	go func() { //this is a GoR to `monitor` if the channel need to be closed
		wg.Wait()
		close(c)
	}()

	for x := range c {
		println(x)
	}

	wg.Wait()

}

// Channels
func goRand(c chan int, wg *sync.WaitGroup) {
	defer wg.Done()
	c <- rand.Intn(10)
}
