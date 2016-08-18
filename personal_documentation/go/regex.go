package main

import (
	"fmt"
	"regexp"
)

func main() {
	fmt.Println("vim-go")
}

func extractHostnamePanic(dataSourceName string) string {
	// This panics if it does not compile
	regex := regexp.MustCompile(`[^:]*:[^:]*@tcp\(([^)]*)\)`)
	// This breaks the regex down into a slice containing the \0 \1 \2 etc...
	// matches.
	result := regex.FindStringSubmatch(dataSourceName)
	return result[1]
}
