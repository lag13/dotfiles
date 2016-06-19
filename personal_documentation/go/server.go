/*
Golang makes it pretty easy to make a server using the net/http package. The
rough process is:

1. Register a bunch of functions of type:

	func(http.ResponseWriter, *http.Request)

which will "handle" the requests made to the server.

2. Call:

	ListenAndServe(addr string, handler Handler)

which will start the server listening for requests on the given addr. I've
only seen addr be the port to listen on like ":8080". I believe that is a
shorthand for doing "0.0.0.0:8080" which means accept connections from
anywhere on port 8080. handler is usually nil in which case the
DefaultServeMux is used.
*/
package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
)

func main() {
	// Doing this registers the "myHandler" function with the DefaultServeMux.
	// This function will be triggered when requests are made to "/". For this
	// example all requests would end up at this handler EXCEPT for those
	// which start with /job since the other handler will grab those first.
	http.HandleFunc("/", myHandler)
	http.HandleFunc("/job", jobGetHandler)
	http.HandleFunc("/json", jsonHandler)
	// This starts the server to listen for request from the localmachine
	// going to port 8080 using the DefaultServeMux. Using no ip address or
	// using 0.0.0.0 means that all incoming connections on port 8080 will be
	// accepted.
	http.ListenAndServe("127.0.0.1:8080", nil)

	// If you do not want to use the DefaultServeMux you can do something like
	// this to create your own *ServeMux, register handlers to it, and pass it
	// into the ListenAndServe() function.
	mux := http.NewServeMux()
	mux.Handle("/", http.HandlerFunc(hh.handleFlagRequest))
	http.ListenAndServe("127.0.0.1:8080", mux)
}

// The purpose of handlers is, of course, to generate the appropriate response
// based on the request that was recieved. As with any http request, the
// appropriate response might be as simple as a status code (for an api
// perhaps) or it could be as complicated as facebook's home page.
func myHandler(w http.ResponseWriter, r *http.Request) {
	log.Print("recieved request for 'myHandler'")
	// You can add custom header information but you'll need to do it before
	// making calls to WriteHeader() or Write(). If no Content-Type is provided
	// then DetectContentType(data []byte) is called on the initial 512 bytes
	// of data. The algorithm is described here:
	// http://mimesniff.spec.whatwg.org. Glancing through it, I don't see
	// anything which sets the application/json header so it seems like you'll
	// have to set that one on your own.
	w.Header().Set("My-Cool-Header", "Wow wow, how cool")
	// w.Header().Set("Content-Type", "text/plain")
	// You can write status codes to the header
	w.WriteHeader(http.StatusBadRequest)
	// You can write to the body of the http response
	fmt.Fprintf(w, "<h1>hello world!</h1>")
	// You can also do it like this
	w.Write([]byte("<h1>hello world!</h1>"))
}

func jobGetHandler(w http.ResponseWriter, req *http.Request) {
	log.Print("recieved request for 'jobGetHandler'")
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(42)
}

func jsonHandler(w http.ResponseWriter, r *http.Request) {
	log.Print("recieved request for 'jsonHandler'")
	w.Header().Set("Content-Type", "application/json")
	m := make(map[string]string)
	// The http.Request object's Body memeber implements the ReadCloser
	// interface. When parsing it as json, a good strategy is to use a Decoder
	// type and use the Decode() method to convert the json string into a type
	// which can then be made use of. This approach is good because a Decoder
	// object draws its data from a type implementing the Read() method which
	// http.Request.Body does.
	d := json.NewDecoder(r.Body)
	var s string
	if err := d.Decode(&s); err != nil {
		fmt.Printf("decoding json: %v", err)
	}
	m["request_body_as_str"] = s
	// What type of HTTP request (GET, POST, PUT, etc...)
	m["method"] = r.Method
	// If the request made to the server was localhost:8080/job/hey/there then
	// r.URL.Path would be /job/hey/there
	m["url_path"] = r.URL.Path
	j, err := json.Marshal(m)
	if err != nil {
		w.WriteHeader(http.StatusBadRequest)
	}
	w.Write(j)
}
