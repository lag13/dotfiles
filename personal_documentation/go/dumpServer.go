// A server which just logs the request it receives.
package main

import (
	"flag"
	"fmt"
	"log"
	"net/http"
	"net/http/httputil"
)

func main() {
	var port = flag.Int("port", 8080, "port to use for the server")
	flag.Parse()
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		rawRequest, err := httputil.DumpRequest(r, true)
		if err != nil {
			fmt.Fprintf(w, "error dumping raw request: %v", err)
			return
		}
		log.Printf("IP of requestor: %q", r.RemoteAddr)
		log.Printf("### New Request ###\n%s###################", rawRequest)
		w.Write([]byte(r.URL.Path))
	})
	log.Print("starting server")
	log.Fatal(http.ListenAndServe(fmt.Sprintf("0.0.0.0:%d", *port), nil))
}
