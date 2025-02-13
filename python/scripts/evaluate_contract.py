#!/usr/bin/env python3

import solcx
from web3 import Web3
# Connect to a local Ethereum node (Ganache, Hardhat, Anvil)
w3 = Web3(Web3.HTTPProvider("http://127.0.0.1:7545"))
assert w3.is_connected(), "Web3 connection failed"

# Install Solidity compiler version
solcx.install_solc("0.8.20")
solcx.set_solc_version("0.8.20")

# Solidity Contracts with Constructor Arguments
contract_1_source = """
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;
contract A {
    uint public value;
    
    constructor(uint _initialValue) {
        require(_initialValue > 0, "Initial value must be greater than zero.");
        value = _initialValue;
    }

    function setValue(uint _value) public { value = _value; }
    function getValue() public view returns (uint) { return value; }
}
"""

contract_2_source = """
// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;
contract B {
    uint public value;
    
    constructor(uint _initialValue) {
        require(_initialValue > 0, "Initial value must be greater than zero.");
        value = _initialValue;
    }

    function setValue(uint _value) public { value = _value; }
    function getValue() public view returns (uint) { return value; }
}
"""

# Compile Solidity contracts
def compile_contract(contract_source):
    compiled_sol = solcx.compile_source(
        contract_source,
        output_values=["abi", "bin"],
        solc_version="0.8.0"
    )
    contract_data = list(compiled_sol.values())[0]
    return contract_data["abi"], contract_data["bin"]

# Compile both contracts
abi1, bin1 = compile_contract(contract_1_source)
abi2, bin2 = compile_contract(contract_2_source)

# Deploy contract function with constructor arguments and error handling
def deploy_contract(abi, bytecode, constructor_args=[]):
    account = w3.eth.accounts[0]
    Contract = w3.eth.contract(abi=abi, bytecode=bytecode)

    try:
        print(f"Deploying contract with args: {constructor_args}")  # Debugging output
        tx_hash = Contract.constructor(*constructor_args).transact({
            "from": account,
            "gas": 3000000  # Increase gas limit
        })
        tx_receipt = w3.eth.wait_for_transaction_receipt(tx_hash)
        print(f"Contract deployed at: {tx_receipt.contractAddress}")  # Deployment success
        return w3.eth.contract(address=tx_receipt.contractAddress, abi=abi)

    except Exception as e:
        print(f"Deployment Error: {e}")  # More detailed error logging
        return None


# Deploy both contracts with constructor arguments (initial value = 10)
contractA = deploy_contract(abi1, bin1, [10])
contractB = deploy_contract(abi2, bin2, [10])

if not contractA or not contractB:
    raise Exception("Contract deployment failed due to an error.")

def test_execution(func_name, args=[]):
    account = w3.eth.accounts[0]

    try:
        # ✅ Check if function takes arguments
        if args:
            tx_hash = contractA.functions[func_name](*args).transact({"from": account})
            w3.eth.wait_for_transaction_receipt(tx_hash)

        # ✅ Call function and fetch result
        resultA = contractA.functions[func_name](*args).call({"from": account})
        resultB = contractB.functions[func_name](*args).call({"from": account})

        # ✅ Compare results
        score = 100 if resultA == resultB else max(0, 100 - abs(resultA - resultB))

        return {"Function": func_name, "Args": args, "ResultA": resultA, "ResultB": resultB, "Score": score}
    
    except Exception as e:
        print(f"Execution Error in {func_name}: {e}")
        return {"Function": func_name, "Args": args, "ResultA": "Error", "ResultB": "Error", "Score": 0}

# Test function with sample inputs
test_cases = [
    ("setValue", [42]),  # Set value
    ("getValue", []),  # Get value
]

results = [test_execution(test[0], test[1]) for test in test_cases]

print(results)