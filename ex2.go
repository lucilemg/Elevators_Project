// Go 1.2
// go run helloworld_go.go

package main

import (
    . "fmt"
    "runtime"
    "time"
    "sync"
)

var i int = 0

func firstGoroutine(c chan int, wg *sync.WaitGroup) {
    for j := 0; j < 1000000; j++ {
		a := <-c
        a++
        c <- a

    }
	wg.Done()
}

func secondGoroutine(c chan int, wg *sync.WaitGroup) {
    for j := 0; j < 1000000; j++ {
		a := <-c
        a--
        c <- a

    }
	wg.Done()
}


func main() {
    c := make(chan int, 1)

    wg := sync.WaitGroup{}

    runtime.GOMAXPROCS(runtime.NumCPU())    // I guess this is a hint to what GOMAXPROCS does...
    

	wg.Add(1) //mark one started                                       // Try doing the exercise both with and without it!
    go firstGoroutine(c, &wg)                      // This spawns someGoroutine() as a goroutine
    wg.Add(1)     
	go secondGoroutine(c, &wg)	

	c <- i

    wg.Wait()

    time.Sleep(100*time.Millisecond)
    Println(i)
}
