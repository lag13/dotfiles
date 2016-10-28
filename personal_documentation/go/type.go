package main

import "fmt"

// If type A can be converted to type B then the converted value C will have
// all the methods of type A. https://golang.org/ref/spec#Conversions

// https://golang.org/ref/spec#Types
type methodNumber int

func (mn methodNumber) echo() {
	fmt.Println("methodNumber = ", mn)
}

// We cannot do this because we cannot define a method on types not defined in
// the current package.
// func (i int) echo() {
// 	fmt.Println("int = ", i)
// }

// A struct is a sequence of named fields, each of which has a name and a type.
// Although struct1 and struct2 look almost the same, they are not equivalent
// types because of struct2 does not have a field called "field2" conversely
// struct 1 does not have a field called "field3". In short structs are
// equivalent if they have the same number of fields, each field has the same
// name, and each field has the same type.
type struct1 struct {
	field1 int
	field2 string
}

func (s1 struct1) echo() {
	fmt.Printf("struct1 = %+v\n", s1)
}

type struct2 struct {
	field1 int
	field3 string
}

type struct3 struct {
	field1 int
	field2 string
}

func main() {
	var mn methodNumber = 10
	mn.echo()

	var n int = 42
	// This will throw an error because int does not implement echo.
	// n.echo()

	// We can convert n to type methodNumber because they have the same
	// underlying type. When the conversion is complete, the converted n has
	// all the methods associated with the methodNumber type.
	methodNumber(n).echo()

	var s1 struct1 = struct1{10, "hello there"}
	s1.echo()

	// This does not work because struct1 and struct2 have different types.
	// s2 := struct2{10, "hi"}
	// struct1(s2).echo()

	// This works because struct1 and struct3 are really the same type.
	var s3 struct3 = struct3{42, "twilight"}
	struct1(s3).echo()

	// A type assertion grabs the concrete value out of an interface.
	var i interface{}
	i = 1
	n = i.(int)
	fmt.Println(n)
	// Trying to do an incompatible type assertion results in a panic. But if
	// you specify another variable (like when getting an argument from a map)
	// that variable will tell you if the type assertion was possible.
	var str string
	str, possibleAssertion := i.(string)
	if !possibleAssertion {
		fmt.Println("type assertion was not possible")
	} else {
		fmt.Println(str)
	}
}
