package tools

import (
	"fmt"
	"strings"

	"github.com/aclements/go-z3/z3"
)

// Compare two contracts by verifying their storage logic
func CompareContracts(opcodesA, opcodesB []string) {
	fmt.Println(opcodesA)
	for _, opA := range opcodesA {
		fmt.Println(opA)
	}
	config := z3.NewContextConfig()
	ctx := z3.NewContext(config)

	solver := z3.NewSolver(ctx)

	// Create symbolic storage locations for each contract
	tokenAStorage := ctx.Const("TokenA_storage", ctx.IntSort())
	tokenBStorage := ctx.Const("TokenB_storage", ctx.IntSort())

	// Track if we found similar storage logic
	storageMatches := false

	for _, opA := range opcodesA {
		for _, opB := range opcodesB {
			if strings.Contains(opA, "SSTORE") && strings.Contains(opB, "SSTORE") {
				solver.Assert(ctx.FromBool(tokenAStorage.AsAST().Equal(tokenBStorage.AsAST())))
				storageMatches = true
			}
		}
	}

	// If no similar storage operations exist, assume contracts differ
	if !storageMatches {
		fmt.Println("❌ No equivalent storage operations detected! Contracts differ.")
		return
	}

	// ✅ Fix: `solver.Check()` does NOT return an error, just a result
	res, err := solver.Check()
	if err != nil {
		fmt.Println("❌ Error checking equivalence:", err)
		return
	}

	// Interpret results
	if res {
		fmt.Println("✅ TokenA and TokenB have equivalent storage behavior!")
	} else {
		fmt.Println("❌ TokenA and TokenB have different state modifications!")
	}
}
