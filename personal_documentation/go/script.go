// You can run executable file (binaries, scripts) by using the os/exec
// package. Using this package could allow you to write scripts in Go rather
// than bash or another language (if you so desire).
// https://nathanleclaire.com/blog/2014/12/29/shelled-out-commands-in-golang/
package main

import (
	"bytes"
	"fmt"
	"log"
	"os"
	"os/exec"
)

// The way the os/exec package works is basically there is this Cmd struct and
// that struct has methods on it to run commands. You could construct this
// struct manually but its probably better if you use the exec.Command()
// function to build it for you.

func main() {
	// os.Args holds a slice of the arguments passed into the command.
	// REMEMBER!! The 0'th spot holds the name of the program being run,
	// starting at position 1 is going to be the first argument.
	fmt.Println(os.Args)
	{ // Run command ignore output 1
		fmt.Println("COMMAND START 1")
		cmd := exec.Command("ls", "-l")
		if err := cmd.Run(); err != nil {
			log.Fatal(err)
		}
		fmt.Println("COMMAND FINISH 1")
	}
	{ // Run command ignore output 2
		fmt.Println("COMMAND START 2")
		// It is a conventional unix thing that the first
		// argument to a command is the command's name.
		// exec.Command() takes care of this detail for you so
		// it's probably better to use that.
		cmd := &exec.Cmd{Path: "/bin/ls", Args: []string{"/bin/ls", "-l"}}
		if err := cmd.Run(); err != nil {
			log.Fatal("running command with manually created Cmd struct: ", err)
		}
		fmt.Println("COMMAND FINISH 2")
	}
	{ // Run command and get output 1
		fmt.Println("COMMAND START 3")
		cmd := "ls"
		args := []string{"-l"}
		output, err := exec.Command(cmd, args...).Output()
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println(string(output))
		fmt.Println("COMMAND FINISH 3")
	}
	{ // Run command and get output 2
		fmt.Println("COMMAND START 4")
		// It is a conventional unix thing that the first
		// argument to a command is the command's name.
		// exec.Command() takes care of this detail for you so
		// it's probably better to use that.
		cmd := &exec.Cmd{Path: "/bin/ls", Args: []string{"/bin/ls", "-l"}}
		output, err := cmd.Output()
		if err != nil {
			log.Fatal(err)
		}
		fmt.Println(string(output))
		fmt.Println("COMMAND FINISH 4")
	}
	// Run command and process error. Note that if you do not manually set the
	// Cmd struct to have a non-nil "Stderr" field and you use the "Run()"
	// command then you will not get any stderr output. If you use the
	// "Output()" command and the "Stderr" field is nil then any stderr will be
	// added to the returned "ExitError" struct's "Stderr" field.
	{
		fmt.Println("COMMAND START 5")
		cmd := exec.Command("rm", []string{"this-file-dont-exist-12345abcde"}...)
		if _, err := cmd.Output(); err != nil {
			if exitErr, ok := err.(*exec.ExitError); ok {
				fmt.Printf("command exited unsuccessfully: %v: %s\n", exitErr, exitErr.Stderr)
			} else {
				fmt.Printf("command exited unsuccessfully: %v\n", err)
			}
		}
		fmt.Println("COMMAND FINISH 5")
	}
	// Process stderr output by setting the Cmd struct's Stderr field. I think
	// the only reason you'd want to manually do this yourself is if you wanted
	// to redirect stderr to say a file or you wanted to ensure you always got
	// all command output (because the builtin way will only return 32 << 10
	// (32768) bytes from the beginning of the error and the same amount of
	// bytes from the end).
	{
		fmt.Println("COMMAND START 6")
		cmd := exec.Command("rm", []string{"this-file-dont-exist-12345abcde"}...)
		var stderr bytes.Buffer
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			fmt.Printf("command exited unsuccessfully: %v: %s\n", err, stderr.Bytes())
		}
		fmt.Println("COMMAND FINISH 6")
	}
}
