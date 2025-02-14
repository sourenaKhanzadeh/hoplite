package main

import (
	"hoplite/tools"
)

func main() {
	// Extract bytecode for both contracts
	opcodesA := tools.ExtractOpcodes("../contracts/sample1.sol")
	opcodesB := tools.ExtractOpcodes("../contracts/sample2.sol")

	// Extract opcodes (storage & computation operations)
	storageOpsA, _ := tools.ExtractStorageOps(opcodesA)
	storageOpsB, _ := tools.ExtractStorageOps(opcodesB)

	// Compare contract logic using Z3
	// Compare contract logic using Z3
	tools.CompareContracts(storageOpsA, storageOpsB)
}
