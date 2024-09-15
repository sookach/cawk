package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec" // Import the package that contains the Command type
	"strings"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintf(os.Stderr, "Usage: cawk-test <filename>")
		os.Exit(1)
	}

	filename := os.Args[1]

	file, err := os.Open(filename)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %s\n", err)
		os.Exit(1)
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
				os.Exit(1)
			}
			command = fmt.Sprintf(line[6:], filename)
		} else if strings.HasPrefix(line, "# expected ") {
			if expected != "" {
				expected += "\n"
			}
			expected += line[11:]
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %s\n", err)
		os.Exit(1)
	}

	if command == "" {
		fmt.Fprintf(os.Stderr, "error: no command provided\n")
		os.Exit(1)
	}

	// Run the command
	cmd := exec.Command("sh", "-c", command)
	cmd.Stdin = os.Stdin
	cmd.Stderr = os.Stderr

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		os.Exit(1)
	}

	if err := cmd.Start(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		os.Exit(1)
	}

	output, err := io.ReadAll(stdout)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		os.Exit(1)
	}

	if err := cmd.Wait(); err != nil {
		fmt.Fprintf(os.Stderr, "error: %s\n", err)
		os.Exit(1)
	}

	if string(output) != expected {
		fmt.Fprintf(os.Stderr, "error: expected %q, got %q\n", expected, string(output))
		os.Exit(1)
	}

	os.Exit(0)
}
