package main

import (
	"bufio"
	"fmt"
	"os"
)

func main() {
	// Can now read things from stdin
	reader := bufio.NewReader(os.Stdin)
	input, err := reader.ReadString('\n')
	// ReadString reads until the first occurrence of delim in the input,
	// returning a string containing the data up to and including the
	// delimiter. If ReadString encounters an error before finding a delimiter,
	// it returns the data read before the error and the error itself (often
	// io.EOF). ReadString returns err != nil if and only if the returned data
	// does not end in delim. For simple uses, a Scanner may be more
	// convenient.
	//
	// So when the error is not nil we can safely remove the delimiter
	// character from the end of the string (assuming we want to do that)
	if err == nil {
		input = input[:len(input)-1]
	}
	fmt.Printf("%q\n", input)
}
