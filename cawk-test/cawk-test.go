package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
)

func test(filename string) bool {
	file, err := os.Open(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		return false
	}
	defer file.Close()

	command, expected := "", ""

	// Read the file line by line
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "# run ") {
			if command != "" {
				fmt.Fprintf(os.Stderr, "error: multiple commands provided\n")
				return false
			}
			command = fmt.Sprintf(line[6:], filename)
		} else if strings.HasPrefix(line, "# out ") {
			if expected != "" {
				expected += "\n"
			}
			expected += line[6:]
		} else if strings.HasPrefix(line, "# output") {
			addNewline := false
			for scanner.Scan() && scanner.Text() != "# output" {
				if !strings.HasPrefix(scanner.Text(), "#") {
					fmt.Fprintf(os.Stderr, "error: unexpected line %q\n", scanner.Text())
					return false
				}

				if addNewline {
					expected += "\n"
				}

				if len(scanner.Text()) > 2 {
					expected += scanner.Text()[2:]
				}

				addNewline = true
			}

			if scanner.Text() != "# output" {
				fmt.Fprintf(os.Stderr, "error: unterminated output block\n")
				return false
			}
			scanner.Scan()
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %s\n", err)
		return false
	}

	if command == "" {
		fmt.Fprintf(os.Stderr, "error: no command provided\n")
		return false
	}

	// Run the command
	cmd := exec.Command("sh", "-c", command)
	cmd.Stdin = os.Stdin
	cmd.Stderr = os.Stderr

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		return false
	}

	if err := cmd.Start(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		return false
	}

	output, err := io.ReadAll(stdout)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		return false
	}

	if err := cmd.Wait(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		return false
	}

	if string(output) != expected {
		fmt.Fprintf(os.Stderr, "error: expected %q, got %q\n", expected, string(output))
		return false
	}

	return true
}

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: cawk-test <filename>")
		os.Exit(1)
	}

	hadError := false
	for _, filename := range os.Args[1:] {
		if !test(filename) {
			hadError = true
		}
	}

	if hadError {
		os.Exit(1)
	}
	os.Exit(0)
}
