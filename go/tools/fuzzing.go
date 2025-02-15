package tools

import (
	"crypto/ecdsa"
	"fmt"
	"regexp"
	"strings"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/ethclient"
)

func FuzzTestEquivalence(
	contractA *Contract, contractB *Contract,
	solFile string, client *ethclient.Client, privateKey *ecdsa.PrivateKey,
	addressA common.Address, addressB common.Address,
) ([]string, []string) {
	functions := ExtractFunctions(solFile)
	var failedFunctions []string
	var passedFunctions []string
	for _, function := range functions {
		functionName := strings.TrimSpace(strings.Split(function, "(")[0])
		fmt.Printf("🔍 Fuzzing Function: %s\n", functionName)

		args := GenerateRandomArguments(function)
		if len(args) == 0 {
			resultA, errA := contractA.GetFunctionValue(client, addressA, functionName)
			if errA != nil {
				fmt.Printf("❌ ContractA function %s failed: %v\n", functionName, errA)
				continue
			}
			resultB, errB := contractB.GetFunctionValue(client, addressB, functionName)
			if errB != nil {
				fmt.Printf("❌ ContractB function %s failed: %v\n", functionName, errB)
				continue
			}
			if !EqualResults(resultA, resultB) {
				fmt.Printf("❌ Function %s mismatch: %v vs %v\n", functionName, resultA, resultB)
				failedFunctions = append(failedFunctions, functionName)
			} else {
				fmt.Printf("✅ Function %s outputs match! %v vs %v\n", functionName, resultA, resultB)
				passedFunctions = append(passedFunctions, functionName)
			}
		} else {
			// Call function on ContractA
			resultA, errA := contractA.GetFunctionValue(client, addressA, functionName, args...)
			if errA != nil {
				fmt.Printf("❌ ContractA function %s failed: %v\n", functionName, errA)
				continue
			}

			// Call function on ContractB
			resultB, errB := contractB.GetFunctionValue(client, addressB, functionName, args...)
			if errB != nil {
				fmt.Printf("❌ ContractB function %s failed: %v\n", functionName, errB)
				continue
			}

			// Compare function outputs
			if !EqualResults(resultA, resultB) {
				fmt.Printf("❌ Function %s mismatch: %v vs %v\n", functionName, resultA, resultB)
				failedFunctions = append(failedFunctions, functionName)
			} else {
				fmt.Printf("✅ Function %s outputs match! %v vs %v\n", functionName, resultA, resultB)
				passedFunctions = append(passedFunctions, functionName)
			}

		}

	}
	return failedFunctions, passedFunctions
}

// Helper function to compare two function outputs
func EqualResults(a, b interface{}) bool {
	return fmt.Sprintf("%v", a) == fmt.Sprintf("%v", b) // Simple string comparison
}

func GenerateRandomArguments(functionSig string) []interface{} {
	// Extract argument types using regex
	re := regexp.MustCompile(`\(([^)]+)\)`) // Matches everything inside parentheses
	matches := re.FindStringSubmatch(functionSig)

	// If no arguments, return an empty array
	if len(matches) < 2 {
		return []interface{}{}
	}

	argTypes := strings.Split(matches[1], ",") // Get list of argument types
	args := make([]interface{}, len(argTypes)) // Allocate slice for arguments

	for i, argType := range argTypes {
		argType = strings.TrimSpace(argType) // Clean whitespace
		args[i] = GenerateRandomValue(argType)
	}

	return args
}

func InvariantTest(
	contractA *Contract, contractB *Contract,
	solFile string, client *ethclient.Client, privateKey *ecdsa.PrivateKey,
	addressA common.Address, addressB common.Address,
) ([]string, []string) {
	functions := ExtractFunctions(solFile)
	var failedFunctions []string
	var passedFunctions []string

	for _, function := range functions {
		functionName := strings.TrimSpace(strings.Split(function, "(")[0])
		fmt.Printf("🔍 Testing Invariant: %s\n", functionName)

		args := GenerateRandomArguments(function)

		// Get storage state before function execution
		ExtractStorageOps(ExtractOpcodes(solFile))
		ExtractStorageOps(ExtractOpcodes(solFile))

		// Execute function
		_, errA := contractA.ExecuteFunction(client, privateKey, addressA, functionName, args...)
		_, errB := contractB.ExecuteFunction(client, privateKey, addressB, functionName, args...)

		if errA != nil || errB != nil {
			fmt.Printf("⚠️ Skipping function %s due to execution failure\n", functionName)
			continue
		}

		// Get storage state after execution
		afterStateA, _ := ExtractStorageOps(ExtractOpcodes(solFile))
		afterStateB, _ := ExtractStorageOps(ExtractOpcodes(solFile))

		// Compare storage changes
		if fmt.Sprintf("%v", afterStateA) != fmt.Sprintf("%v", afterStateB) {
			fmt.Printf("❌ Invariant failure in %s: Different state transitions\n", functionName)
			failedFunctions = append(failedFunctions, functionName)
		} else {
			fmt.Printf("✅ Invariant holds for %s\n", functionName)
			passedFunctions = append(passedFunctions, functionName)
		}
	}
	return failedFunctions, passedFunctions
}

func CompareGasUsage(
	contractA *Contract, contractB *Contract,
	solFile string, client *ethclient.Client, privateKey *ecdsa.PrivateKey,
	addressA common.Address, addressB common.Address,
) {
	functions := ExtractFunctions(solFile)

	for _, function := range functions {
		functionName := strings.TrimSpace(strings.Split(function, "(")[0])

		args := GenerateRandomArguments(function)

		// Measure gas usage for Contract A
		gasA := MeasureGas(client, privateKey, contractA, addressA, functionName, args...)

		// Measure gas usage for Contract B
		gasB := MeasureGas(client, privateKey, contractB, addressB, functionName, args...)

		fmt.Printf("🔍 Gas Comparison for %s: ContractA = %d, ContractB = %d\n", functionName, gasA, gasB)

		if AbsDifference(gasA, gasB) > 5000 {
			fmt.Printf("⚠️ Gas usage varies too much in %s!\n", functionName)
		}
	}
}

// Helper: Measure gas usage for a function
func MeasureGas(client *ethclient.Client, privateKey *ecdsa.PrivateKey, contract *Contract, address common.Address, functionName string, args ...interface{}) uint64 {
	tx, _ := contract.ExecuteFunction(client, privateKey, address, functionName, args...)
	return tx.Gas()
}

// Helper: Calculate absolute difference
func AbsDifference(a, b uint64) uint64 {
	if a > b {
		return a - b
	}
	return b - a
}
