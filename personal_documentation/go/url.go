// URL stuff is kind of tricky with Go because it feels like Go makes
// it very easy to shoot yourself in the foot. Par
package main

import (
	"fmt"
	"net/http"
	"net/url"
)

func main() {
	// I would kind of expect/hope for this to be "smart" and do
	// the query escaping and such so that whoever is recieving
	// this request, the "this" query string parameter will
	// literally be "is+query+str" and "value" will litrally be
	// "hello world". But this function does NOT do any escaping
	// for you so that space in "hello world" will actually just
	// choke the request when you try to send it.
	http.NewRequest(http.MethodGet, "http://hello.com/woa/there?this=is+query+str&value=hello world", nil)
	// So the more proper way to construct a request with query
	// paramters would be something like this:
	var queryStr url.Values = map[string][]string{}
	queryStr.Set("this", "is+query+str")
	queryStr.Set("value", "hello world")
	http.NewRequest(http.MethodGet, fmt.Sprintf("http://hello.com/woa/there?%s", queryStr.Encode()), nil)
	// The problem is that under the hood http.NewRequest calls
	// this function which just takes everything after the '?' and
	// stores it in the RawQuery field. So it doesn't really parse
	// it to check if the query string is valid, it assumes it is.
	u, err := url.Parse("?this=is+query+str&value=hello world")
	if err != nil {
		panic(err)
	}
	fmt.Printf("%#v\n", u)
	// The Query() function parses the RawQuery field it also
	// assumes that the url is properly encoded which means that
	// this will print out: url.Values{"this":[]string{"is query
	// str"}, "value":[]string{"hello world"}}
	fmt.Printf("%#v\n", u.Query())
}
