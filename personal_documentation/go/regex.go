package main

import (
	"fmt"
	"regexp"
)

func main() {
	fmt.Println(extractHostnamePanic("root:@tcp(mysql:3306)/tsrfeatureflags"))
	regex := regexp.MustCompile(`^Prop([0-9]+)`)
	fmt.Println(regex.MatchString("Prop120"))
	fmt.Println(regex.FindStringSubmatch("Prop89"))
}

func extractHostnamePanic(dataSourceName string) string {
	// This panics if it does not compile
	regex := regexp.MustCompile(`[^:]*:[^:]*@tcp\(([^)]*)\)`)
	// This breaks the regex down into a slice containing the \0 \1 \2 etc...
	// matches.
	result := regex.FindStringSubmatch(dataSourceName)
	return result[1]
}
