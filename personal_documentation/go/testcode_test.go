/*
To test code you:

1. For the package you want to test, you make a file ending in _test.go.
2. Import the "testing" package
3. Make a function starting with "Test" which takes a parameter of type
*testing.T.
4. Use the go test command to test a package

Normally you just give a relative path to the package you want to test. So
you can do go test ./package/to/test and it will run. But I think that only
works if its under $GOPATH/src. To test this code it seems that you have to
do go test testcode*.

I kind of like testing in Go because its really just normal go code instead
of some completely new thing.
*/
package testcode

import "testing"

/*
This sort of "table driven" testing is common in go. You define a struct which
has all the parameters you need as well as the expected results. Then you call
the function and check to make sure that you got what was expected.

Test messages are normally of the form:

f(x) = y, want z

If the expected value is a boolean then you can just write the message:

f(x) = y

Because it should be apparent that if you were expecting a boolean and there
was an error message then the value should have been the opposite.
*/
func TestIsEven(t *testing.T) {
	tests := []struct {
		testScenario string
		num          int
		want         bool
	}{
		{"a number is even", 0, false},
		{"a number is odd", 1, false},
		{"a number is odd", 33, false},
		{"a number is even", -2, true},
		{"a number is odd", -1, false},
	}
	for i, test := range tests {
		errorMsg := func(str string, args ...interface{}) {
			t.Helper()
			t.Errorf("Running test %d, where %s:\n"+str, append([]interface{}{i, test.testScenario}, args...)...)
		}
		got := IsEven(test.num)
		if got != test.want {
			errorMsg("the number %d was reported as even=%v", test.num, got)
		}
	}
}

/*
RUN SPECIFIC TEST:

Pass the -run flag to go test to run a specific test. The argument is a regular
expression:

go test -run=MyTest|MyOtherTest ./package

COVERAGE:

Go lets you see how much test coverage you have. This command will output a
percentage of coverage:

	go test -cover ./package/to/test1 ./package/to/test2 ...

Go also lets you visually see what lines of code are under test coverage which
is pretty cool. This is a 2 step process:

	go test -coverprofile=cover.out ./package/to/test
	go tool cover -html=cover.out

This opens up a browser where lines under code coverage are green and lines not
under coverage are red. Note that the name "cover.out" could be anything you
want. It seems that you cannot get coverage for multiple packages at the same
time. So you cannot do:

	go test -coverprofile=cover.out ./package/to/test1 ./package/to/test2 ... <-- CANNOT DO

But you can always test multiple packages with a little bit of bash. For example:

	#!/bin/bash -e
	for pkg in $(go list ./... | grep -v vendor)
	do
		pkg_name=$(echo "$pkg" | tr / -)
		pkg_cov="${pkg_name}.cov"
		go test -v -covermode=count -coverprofile="$pkg_cov" ${pkg}
		if [ -f "$pkg_cov" ]
		then
			go tool cover -html="$pkg_cov" -o $CIRCLE_ARTIFACTS/${pkg_name}.html
		fi
	done

You can even combine the different .cov files into one big one as outlined
here: https://mlafeldt.github.io/blog/test-coverage-in-go/.

ALTERNATE BEHAVIOR BETWEEN REGULAR AND TEST PACKAGE:

You can specify a variable (isTesting or something like that) and set it to
true in the test code. Maybe this could fix a situation where you need some
alternate test code to run when testing otherwise run some normal code.
http://stackoverflow.com/questions/28476307/in-go-how-to-get-test-environment-at-run-time

This package seems useful for getting clear diff outputs of large data
structures like structs: https://github.com/kylelemons/godebug

    t.Errorf("didn't get user info we wanted\n%s", pretty.Compare(got, want))

*/
