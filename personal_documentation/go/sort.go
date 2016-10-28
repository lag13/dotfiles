package main

import "fmt"

func main() {
	fmt.Println("vim-go")
}

// The key to making a sorting thing is to make a new type that represents the collection.
type byWeightDesc []passengerOffer

func (p byWeightDesc) Len() int           { return len(p) }
func (p byWeightDesc) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }
func (p byWeightDesc) Less(i, j int) bool { return p[i].weight > p[j].weight }
