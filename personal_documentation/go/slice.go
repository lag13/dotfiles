package main

import "fmt"

// https://github.com/golang/go/wiki/SliceTricks

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
	// // For a slice a, a[low : high] where 0 <= low <= high <= len(a)
	// // constructs a substring of slice a with length high-low starting at low.
	// // note that if low == len(a) then the empty slice will be reutned
	// // Prints out the slice ["42"]
	// fmt.Println(s[1:2])
	// // Prints out the slice ["one", "42"]
	// fmt.Println(s[0:2])
	// fmt.Println(s[:2])
	// // Prints out the slice s
	// fmt.Println(s[:])
	// fmt.Println(s[:len(s)])
	// Prints out the slice s minus the last element
	fmt.Println(s[:len(s)-1])
	// removing various elements
	fmt.Println("remove element 0:", removeElement(s, 0))
	fmt.Println(s)
	// arr := []string{"one"}
	// half := len(arr) / 2
	// fmt.Println(arr[:half])
	// fmt.Println(arr[half:])
	// fmt.Println(removeElement(s, 1))
	// fmt.Println(s)
	// fmt.Println(removeElement(s, 1))
	// If you know how big the slice should be in memory and what values it
	// should have then the most efficient thing to do would probably be
	// something like this:
	numStuff := 10
	palette := make([]int, numStuff)
	s = make([]string, 0, numStuff)
	for i := 0; i < numStuff; i++ {
		palette = append(palette, i)
	}
	// Of course you'd probably be just fine and dandy to have it start out as
	// an empty slice as well. Computers are fast after all.
}

// A function to remove the specified indice from a slice. 0 <= i <= len(s)-1
func removeElement(s []string, i int) []string {
	s2 := make([]string, len(s))
	copy(s2, s)
	// This still works if i == 0 because in that case then s2[:i] will have
	// low = high = 0 so the length of the slice is 0. This still works if i ==
	// len(s) - 1 because in that case s[:i] = s[0], s[1],... s[len(s)-2]. And
	// s[i+1:] will be the empty slice since i+1 = len(s) and since 'high'
	// defaults to len(s) high-low gives us a slice of length 0.
	return append(s2[:i], s2[i+1:]...)
}
