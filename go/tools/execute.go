package tools

import (
	"crypto/ecdsa"
	"crypto/rand"
	"encoding/hex"
	"fmt"
	"log"
	"math/big"
	"regexp"
	"strconv"
	"strings"

	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/ethclient"
)

func (c *Contract) ExecuteAllFunctions(solFile string, client *ethclient.Client, privateKey *ecdsa.PrivateKey, contractAddress common.Address, defaultArgs map[string][]interface{}) ([]interface{}, map[string][]interface{}, error) {
	functions := ExtractFunctions(solFile)
	re := regexp.MustCompile(`\(([^)]+)\)`) // Extract argument types

	var allArgs map[string][]interface{} = make(map[string][]interface{})
	var values []interface{} // Store return values

	fmt.Printf("✅ Default Args: %v\n", defaultArgs)
	if len(defaultArgs) > 0 {
		for functionName, args := range defaultArgs {
			allArgs[functionName] = args
			if isViewFunction(functionName) {
				result, err := c.GetFunctionValue(client, contractAddress, functionName, args...)
				if err != nil {
					log.Printf("Function execution failed: %v", err)
				} else {
					flattenedResult := FlattenValues(result)
					values = append(values, flattenedResult...)
				}
			} else {
				tx, err := c.ExecuteFunction(client, privateKey, contractAddress, functionName, args...)
				if err != nil {
					log.Printf("Transaction execution failed: %v", err)
				} else {
					log.Printf("✅ Transaction sent for function %s! TX Hash: %s\n", functionName, tx.Hash().Hex())
				}
			}
		}
	} else {
		for _, function := range functions {
			functionName := strings.TrimSpace(strings.Split(function, "(")[0]) // ✅ Trim spaces
			args := []interface{}{}                                            // ✅ Reset args for each function

			// Generate arguments if none were provided
			matches := re.FindStringSubmatch(function)
			if len(matches) > 1 {
				argTypes := strings.Split(matches[1], ",") // ✅ Handle multiple arguments
				for _, argType := range argTypes {
					arg := GenerateRandomValue(strings.TrimSpace(argType)) // ✅ Trim whitespace
					args = append(args, arg)
					fmt.Printf("✅ Function: %s, Arg Type: %s, Arg Value: %v\n", functionName, argType, arg)
				}
			}
			allArgs[functionName] = args // ✅ Store generated args for reuse

			if isViewFunction(functionName) {
				result, err := c.GetFunctionValue(client, contractAddress, functionName, args...)
				if err != nil {
					log.Printf("Function execution failed: %v", err)
				} else {
					flattenedResult := FlattenValues(result)
					values = append(values, flattenedResult...)
				}
			} else {
				tx, err := c.ExecuteFunction(client, privateKey, contractAddress, functionName, args...)
				if err != nil {
					log.Printf("Transaction execution failed: %v", err)
				} else {
					log.Printf("✅ Transaction sent for function %s! TX Hash: %s\n", functionName, tx.Hash().Hex())
				}
			}
		}
	}
	return values, allArgs, nil
}

func FlattenValues(value interface{}) []interface{} {
	switch v := value.(type) {
	case []interface{}:
		return v
	default:
		return []interface{}{v}
	}
}

// ✅ Function to determine if a function is a `view` function
func isViewFunction(functionName string) bool {
	viewFunctions := []string{"getValue", "getValues", "value"} // Add more if needed
	for _, viewFunc := range viewFunctions {
		if functionName == viewFunc {
			return true
		}
	}
	return false
}

// / GenerateRandomValue generates a random value based on the specified Solidity argument type
func GenerateRandomValue(argType string) interface{} {
	switch {
	// ✅ Generalized Integer Handling (uintX, intX)
	case isIntegerType(argType):
		bitSize := extractSize(argType)
		if bitSize <= 0 || bitSize > 256 {
			bitSize = 256 // Default to uint256/int256
		}
		// ✅ Fix: Limit `uint256` to a safe range
		maxValue := new(big.Int).Lsh(big.NewInt(1), uint(bitSize-1)) // Prevent overflow

		n, err := rand.Int(rand.Reader, maxValue)
		if err != nil {
			return nil
		}

		if strings.HasPrefix(argType, "int") { // Handle signed integers
			n.Sub(n, new(big.Int).Rsh(maxValue, 1)) // Shift to handle negatives
		}
		return n

	// ✅ Address
	case argType == "address":
		bytes := make([]byte, 20)
		_, err := rand.Read(bytes) // ✅ FIX: Correct rand.Read usage
		if err != nil {
			return nil
		}
		return common.BytesToAddress(bytes)

	// ✅ Boolean
	case argType == "bool":
		b, err := rand.Int(rand.Reader, big.NewInt(2)) // 0 or 1
		if err != nil {
			return nil
		}
		return b.Int64() == 1

	// ✅ Fixed-size byte arrays (bytes1, bytes16, bytes32)
	case strings.HasPrefix(argType, "bytes") && len(argType) > 5:
		size := extractSize(argType)
		if size > 32 || size <= 0 {
			return nil
		}
		bytes := make([]byte, size)
		_, err := rand.Read(bytes) // ✅ FIX: Correct rand.Read usage
		if err != nil {
			return nil
		}
		return common.BytesToHash(bytes)

	// ✅ Dynamic bytes (bytes)
	case argType == "bytes":
		size := 32 // Default random size
		bytes := make([]byte, size)
		_, err := rand.Read(bytes) // ✅ FIX: Correct rand.Read usage
		if err != nil {
			return nil
		}
		return bytes

	// ✅ String
	case argType == "string":
		size := 10 // Default string size
		bytes := make([]byte, size)
		_, err := rand.Read(bytes) // ✅ FIX: Correct rand.Read usage
		if err != nil {
			return nil
		}
		return hex.EncodeToString(bytes)

	// ✅ Arrays (uint256[], address[], bool[], int256[], etc.)
	case strings.HasSuffix(argType, "[]"):
		baseType := strings.TrimSuffix(argType, "[]") // Extract base type
		arraySize := 3                                // Default array size
		array := make([]interface{}, arraySize)
		for i := 0; i < arraySize; i++ {
			array[i] = GenerateRandomValue(baseType)
		}
		return array

	default:
		return nil // Unsupported type
	}
}

// ✅ Utility function: Check if type is an integer type (uintX, intX)
func isIntegerType(argType string) bool {
	match, _ := regexp.MatchString(`^(u?int)([0-9]*)$`, argType)
	return match
}

// ✅ Utility function: Extract bit size from types like uint256, int128, bytes32
func extractSize(argType string) int {
	re := regexp.MustCompile(`[0-9]+`)
	sizeStr := re.FindString(argType)
	if sizeStr == "" {
		return 256 // Default to 256 bits for integers/bytes
	}
	size, err := strconv.Atoi(sizeStr)
	if err != nil {
		return 256
	}
	return size
}
