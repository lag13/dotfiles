// This can be uploaded to a lambda by following these instructions:
// https://docs.aws.amazon.com/lambda/latest/dg/golang-package.html

// Currently those instructions are:

// GOOS=linux go build main.go

// zip function.zip main

// Upload the function. The handler will always be "main" as far as
// I'm aware. Or maybe you can choose the name of the handler
// function? Eh just make it "main" though. Seems simpler.
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
	"time"

	"github.com/aws/aws-lambda-go/lambda"
)

func getRespBody(httpClient *http.Client, url string) string {
	resp, err := httpClient.Get(url)
	if err != nil {
		return fmt.Sprintf("%+v", err)
	}
	defer resp.Body.Close()
	b, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return fmt.Sprintf("%+v", err)
	}
	return string(b)
}

func HandleRequest() {
	httpClient := &http.Client{
		Timeout: 10 * time.Second,
	}
	fmt.Println(getRespBody(httpClient, "https://elasticsearch.platform.rate.com:9200"))
	fmt.Println(getRespBody(httpClient, "https://elasticsearch.dev.platform.rate.com:9200"))
}

func main() {
	lambda.Start(HandleRequest)
}
