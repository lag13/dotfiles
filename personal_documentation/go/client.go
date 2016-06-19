/*
Using the same net/http package, Golang makes it pretty easy to make a client.
It could be as simple as calling http.Get() or http.Post() with the
appropriate parameters:

	http.Get("http://localhost:8080/hey)
	// The second parameter sets the Content-Type header before sending the
	// data
	http.Post("http://localhost:8080/hey, "application/json", strings.NewReader(`{"this": "content", "is": "cool"}`))

Under the hood, these functions use the DefaultClient object to make their
requests.

If you need a more custom request like changing how redirects are handled or
adding custom headers then you'll have to make your own client and Request
object and call the client's Do() method supplying the request object as a
parameter:

	client := &http.Client{
		CheckRedirect: myCoolRedirectionFunc,
	}
	req, err := http.NewRequest("GET", "http://localhost:8080", nil)
	req.Header.Add("MyCoolHeader", "hey there")
	req.Header.Add("Authorization", "Basic some_really_long_token")
	resp, err := client.Do(req)
*/
package main

import (
	"flag"
	"fmt"
	"net/http"
	"strings"
)

func main() {
	baseURL := "http://localhost:8080"
	url := flag.String("url", "/", "url to send request to")
	method := flag.String("method", "GET", "what type of request to  make")
	flag.Parse()
	var err error
	var resp *http.Response
	completeURL := baseURL + "/" + *url
	switch *method {
	case "GET":
		resp, err = http.Get(completeURL)
	case "POST":
		resp, err = http.Post(completeURL, "application/json", strings.NewReader(`{"hello":42, "how_are":"you"}`))
		defer resp.Body.Close()
	}
	if err != nil {
		fmt.Printf("ERROR: %v", err)
		return
	}
	fmt.Println(resp)
}
