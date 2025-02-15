package main

import (
	"fmt"
	"hoplite/tools"
	"log"
	"math/big"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/ethclient"
)

func main() {
	// Connect to local Ganache
	client, err := ethclient.Dial("http://localhost:7545")
	if err != nil {
		log.Fatalf("Failed to connect to Ethereum client: %v", err)
	}

	// Use first account from Ganache (has 100 ETH by default)
	privateKey, err := crypto.HexToECDSA("04394c7e02e0bcf4edafe448be57bffa047b17f2d2f1728dd200540d9d832108")
	if err != nil {
		log.Fatalf("Failed to parse private key: %v", err)
	}

	contractA, err := tools.NewContract("../contracts/sample1.sol", "Sample1")
	if err != nil {
		log.Fatalf("Failed to create contract A: %v", err)
	}
	contractB, err := tools.NewContract("../contracts/sample2.sol", "Sample2")
	if err != nil {
		log.Fatalf("Failed to create contract B: %v", err)
	}

	// Deploy contracts
	addressA, _, err := contractA.DeployContract(client, privateKey, big.NewInt(1))
	if err != nil {
		log.Fatalf("Failed to deploy contract A: %v", err)
	}
	addressB, _, err := contractB.DeployContract(client, privateKey, big.NewInt(2))
	if err != nil {
		log.Fatalf("Failed to deploy contract B: %v", err)
	}

	failedFunctions, passedFunctions := tools.FuzzTestEquivalence(contractA, contractB, "../contracts/sample1.sol", client, privateKey, addressA, addressB)
	fmt.Printf("Failed Functions: %v\n", failedFunctions)
	fmt.Printf("Passed Functions: %v\n", passedFunctions)
	failedFunctions, passedFunctions = tools.InvariantTest(contractA, contractB, "../contracts/sample1.sol", client, privateKey, addressA, addressB)
	fmt.Printf("Failed Functions: %v\n", failedFunctions)
	fmt.Printf("Passed Functions: %v\n", passedFunctions)
	tools.CompareGasUsage(contractA, contractB, "../contracts/sample1.sol", client, privateKey, addressA, addressB)

}
