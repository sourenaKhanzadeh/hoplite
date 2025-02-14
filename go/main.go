package main

import (
	"log"
	"math/big"

	"hoplite/tools"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/ethclient"
)

func main() {
	client, err := ethclient.Dial("http://127.0.0.1:7545") // Connect to Ganache
	if err != nil {
		log.Fatalf("Failed to connect to Ganache: %v", err)
	}

	privateKey, err := crypto.HexToECDSA("04394c7e02e0bcf4edafe448be57bffa047b17f2d2f1728dd200540d9d832108")
	if err != nil {
		log.Fatalf("Failed to load private key: %v", err)
	}

	contract, err := tools.NewContract("../contracts/sample1.sol", "Sample1")
	if err != nil {
		log.Fatalf("Failed to create contract: %v", err)
	}

	// Deploy contract to Ganache
	address, tx, err := contract.DeployContract(client, privateKey, big.NewInt(100))
	if err != nil {
		log.Fatalf("Deployment failed: %v", err)
	}

	log.Printf("✅ Contract deployed at: %s", address.Hex())
	log.Printf("📌 Transaction hash: %s", tx.Hash().Hex())

	tx, err = contract.ExecuteFunction(client, privateKey, address, "setValue", big.NewInt(100))
	if err != nil {
		log.Fatalf("Function execution failed: %v", err)
	}

	log.Printf("✅ Function executed! TX Hash: %s\n", tx.Hash().Hex())

	value, err := contract.GetFunctionValue(client, address, "getValue")
	if err != nil {
		log.Fatalf("Function execution failed: %v", err)
	}

	log.Printf("💰 Function value: %s", value)

	functions := tools.ExtractFunctions("../contracts/sample1.sol")
	log.Printf("💰 Functions: %v", functions)
}
