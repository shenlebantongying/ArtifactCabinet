package main

import (
	"fmt"
	"math/rand"
	"sync"
)

func main() {

	// Wait everyone before continue, similar to std::latch
	var wg sync.WaitGroup
	c := make(chan int)

	N := 10
	for i := 0; i < N; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			goRand(c)
		}()
	}

	// Note that we need to read the channel so that new values can be added
	for i := 0; i < N; i++ {
		fmt.Println(<-c)
	}
	close(c)

	wg.Wait()
	fmt.Println("Finished")

}

// Channels
func goRand(c chan int) {
	c <- rand.Intn(10)
}
