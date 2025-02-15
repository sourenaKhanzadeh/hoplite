package tools

import (
	"fmt"
	"os/exec"
	"strings"
)

// Extract opcodes and storage operations from bytecode
func ExtractStorageOps(opcodes []string) ([]string, error) {

	// Extract important instructions: SSTORE, SLOAD, ADD, MUL, etc.
	var storageOps []string

	for _, line := range opcodes {
		if strings.Contains(line, "SSTORE") || strings.Contains(line, "SLOAD") ||
			strings.Contains(line, "ADD") || strings.Contains(line, "MUL") ||
			strings.Contains(line, "SUB") || strings.Contains(line, "DIV") {
			storageOps = append(storageOps, strings.TrimSpace(line))
		}
	}

	return storageOps, nil
}

func ExtractOpcodes(solFile string) []string {
	output, err := exec.Command("solc", "--opcodes", solFile).Output()
	if err != nil {
		fmt.Println("Error extracting opcodes:", err)
		return []string{}
	}
	return strings.Split(string(output), "\n")
}
