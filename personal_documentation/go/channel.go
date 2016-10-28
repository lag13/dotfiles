package main

import "fmt"

func main() {
	// Make a channel of errors which has a length of 4. A channel with a
	// length is known as a buffered channel. I think normally you only want to
	// make a buffered channel if you know exactly how many things you want to
	// happen concurrently.
	cerrs := make(chan error, 4)
	fmt.Println(cerrs)
	// An unbuffered channel of ints
	nums := make(chan int)
	// Send a number to the nums channel
	go func() {
		nums <- 1
		fmt.Println("sent 1 on the channel")
	}()
	// recieve the number from the nums channel
	num := <-nums
	fmt.Printf("got %d on the channel\n", num)
}
