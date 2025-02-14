package tools

import (
	"bytes"
	"context"
	"crypto/ecdsa"
	"encoding/json"
	"fmt"
	"log"
	"math/big"
	"os/exec"
	"strings"

	"github.com/ethereum/go-ethereum/accounts/abi"
	"github.com/ethereum/go-ethereum/accounts/abi/bind"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/core/types"
	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/ethclient"
)

// SolidityCompilationOutput handles solc output JSON parsing
type SolidityCompilationOutput struct {
	Contracts map[string]struct {
		ABI json.RawMessage `json:"abi"`
		Bin string          `json:"bin"`
	} `json:"contracts"`
}

// Contract struct holds ABI & Bytecode in memory
type Contract struct {
	ABI        abi.ABI
	Bytecode   []byte
	Transactor *bind.BoundContract
}

// NewContract compiles a Solidity contract and extracts ABI & Bytecode
func NewContract(solFilePath string, contractName string) (*Contract, error) {
	// Run solc to compile the contract
	cmd := exec.Command("solc", "--combined-json", "abi,bin", solFilePath)
	var out bytes.Buffer
	cmd.Stdout = &out

	err := cmd.Run()
	if err != nil {
		return nil, fmt.Errorf("failed to compile Solidity contract: %v", err)
	}

	// Parse solc output
	var output SolidityCompilationOutput
	err = json.Unmarshal(out.Bytes(), &output)
	if err != nil {
		return nil, fmt.Errorf("failed to parse solc output: %v", err)
	}

	// Extract ABI and Bytecode
	contractKey := fmt.Sprintf("%s:%s", solFilePath, contractName)
	contractData, exists := output.Contracts[contractKey]
	if !exists {
		return nil, fmt.Errorf("contract %s not found in solc output", contractName)
	}

	// Convert ABI from json.RawMessage to string
	abiStr := string(contractData.ABI)

	// Parse ABI
	parsedABI, err := abi.JSON(strings.NewReader(abiStr))
	if err != nil {
		return nil, fmt.Errorf("failed to parse contract ABI: %v", err)
	}

	// Convert bytecode from hex string to bytes
	bytecode := common.FromHex(contractData.Bin)

	return &Contract{
		ABI:      parsedABI,
		Bytecode: bytecode,
	}, nil
}

// ✅ Fixed: Execute smart contract function on a deployed contract
func (c *Contract) ExecuteFunction(client *ethclient.Client, privateKey *ecdsa.PrivateKey, contractAddress common.Address, functionName string, args ...interface{}) (*types.Transaction, error) {
	// Create a transactor
	auth, err := bind.NewKeyedTransactorWithChainID(privateKey, big.NewInt(1337)) // Ganache Chain ID
	if err != nil {
		return nil, fmt.Errorf("failed to create transactor: %v", err)
	}

	// Get nonce **from sender's account**, NOT from the contract
	fromAddress := crypto.PubkeyToAddress(privateKey.PublicKey)
	nonce, err := client.PendingNonceAt(context.Background(), fromAddress)
	if err != nil {
		return nil, fmt.Errorf("failed to get nonce: %v", err)
	}

	auth.Nonce = big.NewInt(int64(nonce))
	auth.Value = big.NewInt(0)              // No ETH sent
	auth.GasLimit = uint64(3000000)         // Gas limit
	auth.GasPrice = big.NewInt(20000000000) // Set low gas price (20 Gwei)

	// ✅ Fixed: Ensure `Transactor` is properly initialized before calling `Transact()`
	if c.Transactor == nil {
		c.Transactor = bind.NewBoundContract(contractAddress, c.ABI, client, client, client)
	}

	// ✅ Call contract function
	tx, err := c.Transactor.Transact(auth, functionName, args...)
	if err != nil {
		return nil, fmt.Errorf("failed to execute function: %v", err)
	}

	return tx, nil
}

func (c *Contract) GetFunctionValue(client *ethclient.Client, contractAddress common.Address, functionName string, args ...interface{}) ([]interface{}, error) {
	// ✅ Ensure Transactor is properly initialized
	if c.Transactor == nil {
		c.Transactor = bind.NewBoundContract(contractAddress, c.ABI, client, client, client)
	}

	// ✅ Prepare call options (Read-Only Call)
	callOpts := &bind.CallOpts{
		Pending: false,
		From:    contractAddress, // Any address works for read functions
	}

	// ✅ Use a slice to store multiple return values
	var result []interface{}

	// ✅ Call contract function correctly
	err := c.Transactor.Call(callOpts, &result, functionName, args...)
	if err != nil {
		return nil, fmt.Errorf("failed to call function %s: %v", functionName, err)
	}

	return result, nil
}

// DeployContract deploys a smart contract with constructor arguments
func (c *Contract) DeployContract(client *ethclient.Client, privateKey *ecdsa.PrivateKey, constructorArgs ...interface{}) (common.Address, *types.Transaction, error) {
	// Use ABI directly since it's already parsed
	parsedABI := c.ABI

	// Create a transactor
	auth, err := bind.NewKeyedTransactorWithChainID(privateKey, big.NewInt(1337)) // Ganache chain ID is 1337
	if err != nil {
		return common.Address{}, nil, fmt.Errorf("failed to create transactor: %v", err)
	}

	// Get sender address
	fromAddress := crypto.PubkeyToAddress(privateKey.PublicKey)

	// Get nonce
	nonce, err := client.PendingNonceAt(context.Background(), fromAddress)
	if err != nil {
		return common.Address{}, nil, fmt.Errorf("failed to get nonce: %v", err)
	}

	// Set gas price and limit (Ganache allows 0 gas price)
	auth.Nonce = big.NewInt(int64(nonce))
	auth.Value = big.NewInt(0)              // No ETH sent
	auth.GasLimit = uint64(3000000)         // Gas limit
	auth.GasPrice = big.NewInt(20000000000) // Set low gas price (20 Gwei)

	// ✅ Fix: Use `bind.DeployContract()` **without constructor arguments**
	address, tx, _, err := bind.DeployContract(auth, parsedABI, c.Bytecode, client, constructorArgs...)
	if err != nil {
		return common.Address{}, nil, fmt.Errorf("failed to deploy contract: %v", err)
	}

	// Print TX hash
	log.Printf("Deploying contract to Ganache... TX Hash: %s\n", tx.Hash().Hex())

	// Return contract address & transaction
	return address, tx, nil
}
