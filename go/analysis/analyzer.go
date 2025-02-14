package analysis

import (
	"hoplite/tools"
	"log"
	"math"
	"math/big"
	"reflect"

	"github.com/ethereum/go-ethereum/crypto"
	"github.com/ethereum/go-ethereum/ethclient"
)

func AnalyzeContracts(solFile1 string, solFile2 string) {
	client, err := ethclient.Dial("http://127.0.0.1:7545") // Connect to Ganache
	if err != nil {
		log.Fatalf("Failed to connect to Ganache: %v", err)
	}

	privateKey, err := crypto.HexToECDSA("04394c7e02e0bcf4edafe448be57bffa047b17f2d2f1728dd200540d9d832108")
	if err != nil {
		log.Fatalf("Failed to load private key: %v", err)
	}

	contract1Name, err := tools.GetContractName(solFile1)
	if err != nil {
		log.Fatalf("Failed to get contract 1 name: %v", err)
	}
	contract2Name, err := tools.GetContractName(solFile2)
	if err != nil {
		log.Fatalf("Failed to get contract 2 name: %v", err)
	}

	// Load contracts
	contract1, err := tools.NewContract(solFile1, contract1Name)
	if err != nil {
		log.Fatalf("Failed to load contract 1: %v", err)
	}

	contract2, err := tools.NewContract(solFile2, contract2Name)
	if err != nil {
		log.Fatalf("Failed to load contract 2: %v", err)
	}

	// Deploy contracts
	address1, tx1, err := contract1.DeployContract(client, privateKey, big.NewInt(100))
	if err != nil {
		log.Fatalf("Failed to deploy contract 1: %v", err)
	}

	address2, tx2, err := contract2.DeployContract(client, privateKey, big.NewInt(100))
	if err != nil {
		log.Fatalf("Failed to deploy contract 2: %v", err)
	}

	log.Printf("✅ Contract 1 deployed! Address: %s, TX Hash: %s", address1.Hex(), tx1.Hash().Hex())
	log.Printf("✅ Contract 2 deployed! Address: %s, TX Hash: %s", address2.Hex(), tx2.Hash().Hex())

	var args1 map[string][]interface{} = make(map[string][]interface{})
	// Execute all functions
	values1, args1, err := contract1.ExecuteAllFunctions(solFile1, client, privateKey, address1, args1)
	if err != nil {
		log.Fatalf("Failed to execute functions for contract 1: %v", err)
	}

	values2, _, err := contract2.ExecuteAllFunctions(solFile2, client, privateKey, address2, args1)
	if err != nil {
		log.Fatalf("Failed to execute functions for contract 2: %v", err)
	}

	log.Printf("✅ Contract 1 executed! Values: %v", values1)
	log.Printf("✅ Contract 2 executed! Values: %v", values2)

	// Compare values and score the contracts
	score1 := CompareValues(values1, values2)
	score2 := CompareValues(values2, values1)

	log.Printf("✅ Contract 1 score: %f", score1)
	log.Printf("✅ Contract 2 score: %f", score2)
}

// CompareValues compares two slices of values and returns a similarity score (0-1).
func CompareValues(values1 []interface{}, values2 []interface{}) float64 {
	score := 0
	minLength := int(math.Min(float64(len(values1)), float64(len(values2))))

	for i := 0; i < minLength; i++ {
		if reflect.DeepEqual(tools.FlattenValues(values1[i]), tools.FlattenValues(values2[i])) {
			score++
		}
	}

	if minLength == 0 {
		return 0 // Avoid division by zero
	}
	return float64(score) / float64(minLength) // ✅ Return float ratio
}
