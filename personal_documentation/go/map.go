package main

func main() {
	// The "zero" value for a map is nil. This kind of map cannot be used yet
	// because we'll get a nil-pointer reference error.
	var m map[string]bool
	// We can initialize an empty map like this.
	m = make(map[string]bool)
	// We can set a map to an initial default value like this
	m = map[string]bool{
		"one":      true,
		"42":       false,
		"heythere": true,
		"you":      true,
	}
}

// The easiest way to copy a map is just with a loop. There is no built in map
// copy operator.
func copyMap(m map[string]bool) map[string]bool {
	cm := make(map[string]bool)
	for k, v := range m {
		cm[k] = v
	}
	return cm
}
