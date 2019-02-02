package main

import (
	"flag"
	"fmt"
	"strings"
)

type SortType string

func (s SortType) String() string {
	return string(s)
}

var validSortTypes = []string{"gender-asc", "lastname-desc"}

func contains(xs []string, y string) bool {
	for _, x := range xs {
		if x == y {
			return true
		}
	}
	return false
}

func (s *SortType) Set(str string) error {
	if !contains(validSortTypes, str) {
		return fmt.Errorf("invalid value, allowed values are %s", strings.Join(validSortTypes, ", "))
	}
	*s = SortType(str)
	return nil
}

func main() {
	// hello := flag.String("hello", "", "some usage stuff")
	// n := flag.Int("n", 0, "asdf")
	var sortType SortType = "default wooo!"
	flag.Var(&sortType, "sort", "type of sorting to do")
	fmt.Printf("%+v\n", flag.Lookup("sort"))
	flag.Parse()
	fmt.Printf("%+v\n", flag.Lookup("sort"))
	// fmt.Printf("%q\n", *hello)
	// fmt.Println(*n)
	fmt.Println(sortType)
}
