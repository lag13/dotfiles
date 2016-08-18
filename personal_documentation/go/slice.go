package main

func main() {
	// The "zero" value for a slice is nil. A slice is really a pointer to a struct containing three things:
	// 1. The array which actually holds the values
	// 2. A "length" value that is the length of the slice
	// 3. A "capacity" value which is the size of the array.
	var s []string
	// We can initialize an empty slice like this.
	s = []string{}
	// Or this
	s = make([]string, 0)
	// We can give the slice an initial length and capacity of 5 like this
	s = make([]string, 5)
	// This one has initial length of 5 and capacity 10
	s = make([]string, 5, 10)
	// This one has initial length of 0 and capacity 10
	s = make([]string, 0, 10)
	// We can set a slice to an initial default value like this
	s = []string{
		"one",
		"42",
		"heythere",
		"you",
	}
	// If you know how big the slice should be in memory and what values it
	// should have then the most efficient thing to do would probably be
	// something like this:
	numStuff := 10
	s = make([]string, 0, numStuff)
	for i := 0; i < numStuff; i++ {
		palette = append(palette, i)
	}
	// Of course you'd probably be just fine and dandy to have it start out as
	// an empty slice as well. Computers are fast after all.
}
