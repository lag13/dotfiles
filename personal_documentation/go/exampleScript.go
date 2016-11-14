package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"regexp"
)

const usage = `
Name:
  deleteAttachments

Synopsis:
  deleteAttachments (dev|us|eu|sg) <customer-name>

Description:
  Delete attachments for the customer "customer-name" on the specifed datacenter.
  
  dev|us|eu|sg
	Specifies which bucket the attachments get deleted from.
  
  <customer-name>
	The name of the folder which has all the customer attachments. As far as
	I've seen, the name is the database name.

Examples:
  deleteAttachments dev weisersecurityservic
  deleteAttachments us ussugar
`

func main() {
	if len(os.Args)-1 != 2 {
		fmt.Println(usage)
		return
	}
	var attachmentBucket string
	{ // Get the bucket to delete attachments from
		whichDatacenter := os.Args[1]
		mapping := map[string]string{
			"dev": "some-attachments-dev",
			"us":  "some-attachments-us-prod",
			"eu":  "some-attachments-eu-prod",
			"sg":  "some-attachments-sg-prod",

			// For testing purposes only
			"test":  "lucas-dev-testing-customer-attachments",
			"testr": "lucas-dev-testing-attachments",
		}
		var ok bool
		if attachmentBucket, ok = mapping[whichDatacenter]; !ok {
			fmt.Println(usage)
			return
		}
	}
	whichCS := os.Args[2]
	{ // Verify second argument is a valid customer system name (i.e. prevent me from doing something stupid)
		if matched, err := regexp.MatchString("^[a-zA-Z0-9-_]+$", whichCS); err != nil || !matched {
			if err != nil {
				log.Fatal("problem with the regex: ", err)
			}
			fmt.Println("customer name can only contain alphanumeric characters and dashes")
			return
		}
	}
	attachmentsS3URI := fmt.Sprintf("s3://%s/%s/", attachmentBucket, whichCS)
	args := []string{"s3", "rm", "--recursive", attachmentsS3URI}
	cmd := exec.Command("aws", args...)
	_, err := cmd.Output()
	if err != nil {
		if exitErr, ok := err.(*exec.ExitError); ok {
			fmt.Printf("command exited unsuccessfully: %v: %s\n", exitErr, exitErr.Stderr)
		} else {
			fmt.Printf("command could not be started: %v\n", err)
		}
		return
	}
}
