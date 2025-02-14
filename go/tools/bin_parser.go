package tools

import (
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
)

// GetBytecode compiles a Solidity contract and returns its bytecode.
func GetBytecode(solFile string) (string, error) {
	// Define the output directory and file paths
	outputDir := "./build"
	os.RemoveAll(outputDir)

	// Create the output directory if it doesn't exist
	if err := os.MkdirAll(outputDir, 0755); err != nil {
		return "", fmt.Errorf("failed to create output directory: %v", err)
	}

	// Compile the Solidity contract
	cmd := exec.Command("solc", "--bin", "--output-dir", outputDir, solFile, "--overwrite")
	if output, err := cmd.CombinedOutput(); err != nil {
		return "", fmt.Errorf("compilation failed: %v\nOutput: %s", err, string(output))
	}

	binFile, err := os.ReadDir(outputDir)
	if err != nil {
		return "", fmt.Errorf("failed to read directory: %v", err)
	}
	for _, file := range binFile {
		if !strings.HasSuffix(file.Name(), ".bin") {
			continue
		}
		bytecode, err := os.ReadFile(outputDir + "/" + file.Name())
		if err != nil {
			return "", fmt.Errorf("failed to read bytecode file: %v", err)
		}
		return string(bytecode), nil
	}

	return "", fmt.Errorf("failed to read bytecode file")
}

// Extract function signatures from EVM bytecode
func ExtractFunctionSignatures(bytecode string) []string {
	if len(bytecode) < 8 {
		fmt.Println("Bytecode too short to extract function selectors")
		return []string{}
	}

	// Function selectors are the first 4 bytes of function calls in EVM
	var selectors []string
	for i := 0; i < len(bytecode)-8; i += 2 {
		selectors = append(selectors, "0x"+bytecode[i:i+8])
	}
	return selectors
}

func ExtractOpcodes(solFile string) []string {
	output, err := exec.Command("solc", "--opcodes", solFile).Output()
	if err != nil {
		fmt.Println("Error extracting opcodes:", err)
		return []string{}
	}
	return strings.Split(string(output), "\n")
}

func ExtractFunctions(solFile string) []string {
	output, err := exec.Command("solc", "--hashes", solFile).Output()
	if err != nil {
		fmt.Println("Error extracting functions:", err)
		return []string{}
	}
	functions := strings.Split(string(output), "\n")
	var names []string
	// regex to filter functions start with 0x and not start with 0x0
	re := regexp.MustCompile(`^([A-Fa-f0-9]+): ([A-Za-z()0-9])+`)
	for _, function := range functions {
		if !re.MatchString(function) {
			continue
		}
		names = append(names, strings.Split(function, ":")[1])
	}
	return names
}
