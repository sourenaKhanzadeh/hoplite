package tools

import (
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
