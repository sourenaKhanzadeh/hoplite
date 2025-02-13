#!/usr/bin/env python3
import random
import string
import pathlib
import re
import sys

sys.path.append(str(pathlib.Path(__file__).parent.parent))

from utils.big_numbers import CompareBigDigits
from difflib import SequenceMatcher
import solcx
from web3 import Web3
from eth_utils import to_checksum_address, is_address

class Node:
    def __init__(self, contract_source: pathlib.Path, constructor_args=[], version="0.8.0"):
        # ✅ Connect to Ethereum Node (Ganache, Hardhat, Anvil)
        self.w3 = Web3(Web3.HTTPProvider("http://127.0.0.1:7545"))
        assert self.w3.is_connected(), "Web3 connection failed"

        # ✅ Install Solidity Compiler
        solcx.install_solc(version)
        solcx.set_solc_version(version)

        # ✅ Read Solidity Contract
        with open(contract_source, "r") as f:
            contract_source = f.read()

        # ✅ Compile Contract
        self.abi, self.bytecode = self.compile_contract(contract_source)
        # ✅ Deploy Contract and Check If Successful
        self.contract = self.deploy_contract(constructor_args)
        if self.contract is None:
            raise Exception("❌ Contract deployment failed! Check Ganache logs.")

    def compile_contract(self, contract_source):
        compiled_sol = solcx.compile_source(
            contract_source,
            output_values=["abi", "bin"]
        )
        contract_data = list(compiled_sol.values())[0]
        return contract_data["abi"], contract_data["bin"]

    def deploy_contract(self, constructor_args=[]):
        account = self.w3.eth.accounts[0]
        Contract = self.w3.eth.contract(abi=self.abi, bytecode=self.bytecode)

        try:
            print(f"🚀 Deploying contract with args: {constructor_args}")  # Debugging output
            
            tx_hash = Contract.constructor(*constructor_args).transact({
                "from": account,
                "gas": 6_000_000  # ✅ Increased Gas Limit
            })
            tx_receipt = self.w3.eth.wait_for_transaction_receipt(tx_hash)

            print(f"✅ Contract deployed at: {tx_receipt.contractAddress}")  # Deployment success
            return self.w3.eth.contract(address=tx_receipt.contractAddress, abi=self.abi)

        except Exception as e:
            print(f"❌ Deployment Error: {e}")  # More detailed error logging
            return None

    def execute_contract(self, func_args_map={}):
        """
        Executes all functions of the contract with correct argument unpacking.
        """
        if self.contract is None:
            raise Exception("❌ Contract is not deployed! Execution cannot proceed.")

        account = self.w3.eth.accounts[0]
        results = {}

        for func_name, args in func_args_map.items():
            func = getattr(self.contract.functions, func_name)

            try:
                # ✅ Fix argument unpacking
                if args:
                    tx_hash = func(*args).transact({"from": account})
                    tx_receipt = self.w3.eth.wait_for_transaction_receipt(tx_hash)
                    print(f"✅ Function {func_name} executed successfully.")
                    results[func_name] = tx_receipt.transactionHash.hex()
                else:
                    result = func().call({"from": account})
                    print(f"📌 Function {func_name} returned: {result}")
                    results[func_name] = result

            except Exception as e:
                print(f"❌ Execution Error in {func_name}: {e}")
                results[func_name] = "Error"

        return results



class Evaluator:
    def __init__(self, nodeA: Node, nodeB: Node):
        self.nodeA = nodeA
        self.nodeB = nodeB

    def evaluate_contracts(self):
        """Only evaluate functions that return a value."""
        functions_with_returnA = get_functions_with_return_values(self.nodeA.contract)
        functions_without_returnA = get_functions_without_return_values(self.nodeA.contract)
        print("Functions with return values:", functions_with_returnA)
        print("Functions without return values:", functions_without_returnA)

        resultA = {}
        get_args = {}
        for func_name, args in functions_without_returnA.items():
            get_args[func_name] = generate_based_on_args(args)
            self.nodeA.execute_contract({func_name: get_args[func_name]})

        for func_name, args in functions_with_returnA.items():
            get_args[func_name] = generate_based_on_args(args)
            resultA[func_name] = self.nodeA.execute_contract({func_name: get_args[func_name]})

        resultB = {}
        functions_with_returnB = get_functions_with_return_values(self.nodeB.contract)
        functions_without_returnB = get_functions_without_return_values(self.nodeB.contract)
        for func_name, args in functions_without_returnB.items():
            self.nodeB.execute_contract({func_name: get_args[func_name]})
        
        for func_name, args in functions_with_returnB.items():
            resultB[func_name] = self.nodeB.execute_contract({func_name: get_args[func_name]})


        return resultA, resultB


    def compare_results(self, valueA, valueB):
        """ Compare results and return a similarity score (0-1), allowing partial numerical matches. """

        # ✅ Extract return values from execution results if they exist
        if isinstance(valueA, dict):
            valueA = valueA[list(valueA.keys())[0]]
        if isinstance(valueB, dict):
            valueB = valueB[list(valueB.keys())[0]]

        # if is_address(valueA) and is_address(valueB):
            # return 1.0  # ✅ Ignore address differences (full score)

        

        if isinstance(valueA, (int, float)) and isinstance(valueB, (int, float)):
            ans = CompareBigDigits(valueA, valueB)
            result = ans.exec()
            return 1.0 if result == 0 else 0.0
            

        elif isinstance(valueA, str) and isinstance(valueB, str):
            # ✅ Use string similarity for fuzzy matching
            return SequenceMatcher(None, valueA, valueB).ratio()

        elif isinstance(valueA, bool) and isinstance(valueB, bool):
            return 1.0 if valueA == valueB else 0.0  # ✅ Boolean exact match

        elif isinstance(valueA, list) and isinstance(valueB, list):
            # ✅ Compare lists element-wise, allowing for numerical differences
            min_length = min(len(valueA), len(valueB))
            max_length = max(len(valueA), len(valueB), 1)  # Avoid division by zero
            element_scores = [
                self.compare_results(valueA[i], valueB[i]) for i in range(min_length)
            ]
            return sum(element_scores) / max_length  # Average similarity score
        
        elif valueA == valueB:
            return 1.0  # ✅ Exact match = full points

        return 0  # ❌ Completely different types → No match


    def eval_score(self):
        resultA, resultB = self.evaluate_contracts()
        total_score = 0
        function_count = len(resultA)

        for func_name in resultA:
            if func_name in resultB:
                score = self.compare_results(resultA[func_name], resultB[func_name])
                total_score += score
        print("function_count", function_count)
        return (total_score / function_count) * 100 if function_count > 0 else 0  # ✅ Percentage-based score



def get_functions_with_args(contract):
    functions_with_args = {}

    for func_abi in contract.abi:  # ✅ Iterate over full contract ABI
        if func_abi["type"] == "function" and func_abi["inputs"]:  # ✅ Only functions with inputs
            func_name = func_abi["name"]
            arg_details = [(inp["name"], inp["type"]) for inp in func_abi["inputs"]]
            functions_with_args[func_name] = arg_details

    return functions_with_args

def get_functions_with_return_values(contract):
    """Get functions that return a value (skip void functions)."""
    functions_with_return = {}

    for func_abi in contract.abi:
        if func_abi["type"] == "function" and func_abi.get("outputs", []):  # ✅ Skip void functions
            func_name = func_abi["name"]
            arg_details = [(inp["name"], inp["type"]) for inp in func_abi["inputs"]]
            functions_with_return[func_name] = arg_details  # ✅ Keep only functions that return a value

    return functions_with_return


def get_functions_without_return_values(contract):
    functions_without_return = {}
    for func_abi in contract.abi:
        if func_abi["type"] == "function" and not func_abi.get("outputs", []):
            func_name = func_abi["name"]
            arg_details = [(inp["name"], inp["type"]) for inp in func_abi["inputs"]]
            functions_without_return[func_name] = arg_details
    return functions_without_return

def generate_based_on_args(args):
    generated_args = []

    for arg_name, arg_type in args:
        if "uint" in arg_type:  # ✅ Handle unsigned integers (uint8 - uint256)
            match = re.search(r'uint(\d+)', arg_type)  # Extract size (e.g., uint16 → 16)
            bit_size = int(match.group(1)) if match else 256  # Default to uint256 if no size given
            max_value = 2**bit_size - 1  # Max possible value for uintN
            generated_args.append(random.randint(0, max_value))

        elif "int" in arg_type:  # ✅ Handle signed integers (int8 - int256)
            match = re.search(r'int(\d+)', arg_type)  # Extract size
            bit_size = int(match.group(1)) if match else 256
            min_value = -2**(bit_size - 1)  # Min value
            max_value = 2**(bit_size - 1) - 1  # Max value
            generated_args.append(random.randint(min_value, max_value))

        elif "bool" in arg_type:  # ✅ Generate random boolean
            generated_args.append(random.choice([True, False]))

        elif "string" in arg_type:  # ✅ Generate random string (10 chars)
            generated_args.append(''.join(random.choices(string.ascii_letters + string.digits, k=10)))

        elif "address" in arg_type:  # ✅ Generate random Ethereum address
            random_address = "0x" + ''.join(random.choices("0123456789abcdef", k=40))
            generated_args.append(to_checksum_address(random_address))

        else:
            print(f"⚠️ Unsupported type: {arg_type}, using None")
            generated_args.append(None)  # Default to None for unsupported types

    return generated_args


if __name__ == "__main__":
    try:
        nodeA = Node(pathlib.Path(__file__).parent.parent.parent / "contracts" / "sample1.sol", [10])  # ✅ Constructor Argument Fix
        nodeB = Node(pathlib.Path(__file__).parent.parent.parent / "contracts" / "sample2.sol", [10])  # ✅ Constructor Argument Fix
        evaluator = Evaluator(nodeA, nodeB)
        score = evaluator.eval_score()
        print(f"Score: {score}")
    except Exception as e:
        print(f"❌ Error during execution: {e}")
