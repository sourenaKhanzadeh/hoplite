import pathlib
import hashlib
import random
import subprocess
import solcx
import re
from web3 import Web3
from contract import Contract
from slither.slither import Slither
from solc_select import solc_select


class CFG:
    def __init__(self, contract_pth: pathlib.Path):
        version = Contract.get_contract_version(contract_pth)
        solc_select.switch_global_version(version, always_install=True)
        solcx.install_solc(version)
        solcx.set_solc_version(version)
        self.contract_pth = contract_pth
        self.sl = Slither(str(contract_pth))
        self.name = self.sl.contracts[0].name

        # Extract function metadata
        self.functions_cfg = self.extract_functions_cfg()
        self.functions_ast = self.extract_functions_ast()
        self.functions_bytecode, self.abi = self.get_function_bytecodes()

    def extract_functions_cfg(self):
        """Extracts function names and CFG hashes for comparison."""
        functions_cfg = {}
        for contract in self.sl.contracts:
            for function in contract.functions:
                cfg_string = self.get_cfg_string(function)
                functions_cfg[function.full_name] = self.hash_cfg(cfg_string)
        return functions_cfg

    def extract_functions_ast(self):
        """Extracts function names and AST hashes for comparison."""
        functions_ast = {}
        for contract in self.sl.contracts:
            for function in contract.functions:
                ast_string = self.get_ast_string(function)
                functions_ast[function.full_name] = self.hash_cfg(ast_string)
        return functions_ast

    @staticmethod
    def get_cfg_string(function):
        """Generates a unique string representation of the function's CFG."""
        cfg_repr = []
        for node in function.nodes:
            expressions = node.expression if node.expression else "EMPTY"
            cfg_repr.append(f"{node.node_id}: {expressions}")
        return "\n".join(cfg_repr)

    @staticmethod
    def get_ast_string(function):
        """Generates a unique string representation of the function's AST."""
        return str(function.solidity_signature)

    @staticmethod
    def hash_cfg(cfg_string):
        """Creates a hash from the CFG string to compare functions efficiently."""
        return hashlib.md5(cfg_string.encode()).hexdigest()

    def get_function_bytecodes(self):
        """Compiles the contract and extracts function-specific bytecodes using solcx."""
        try:
            with open(self.contract_pth, "r") as f:
                source_code = f.read()

            compiled = solcx.compile_standard(
                {
                    "language": "Solidity",
                    "sources": {self.contract_pth.name: {"content": source_code}},
                    "settings": {
                        "optimizer": {"enabled": True, "runs": 200},  # Ensure consistency
                        "outputSelection": {"*": {"*": ["evm.deployedBytecode", "abi"]}},  # Extract deployed bytecode
                    },
                }
            )

            function_bytecodes = {}
            abi = None
            for contract_name, contract_data in compiled["contracts"][self.contract_pth.name].items():
                bytecode = contract_data["evm"]["deployedBytecode"]["object"]
                bytecode = self.strip_metadata(bytecode)  # Strip metadata to avoid false mismatches
                function_bytecodes[contract_name] = bytecode
                abi = contract_data["abi"]

            return function_bytecodes, abi
        except Exception as e:
            print(f"[ERROR] Error extracting function bytecodes: {e}")
            return {}, None


    import re

    @staticmethod
    def strip_metadata(bytecode):
        """Strips Solidity compiler metadata and contract-specific differences from bytecode."""
        # Strip metadata (last 43 bytes in hex are compiler metadata)
        if len(bytecode) > 86:
            bytecode = bytecode[:-86]

        # Replace contract name references (avoids minor name-based differences)
        bytecode = re.sub(r"6060[0-9a-f]{2}573[0-9a-f]{4}", "6060XX573XXX", bytecode)

        return bytecode



    def get_constructor_args(self, abi):
        """Generates random values for the constructor arguments based on the ABI."""
        constructor_args = []
        for item in abi:
            if item.get("type") == "constructor":
                inputs = item.get("inputs", [])
                for param in inputs:
                    param_type = param["type"]
                    if "uint" in param_type:
                        constructor_args.append(random.randint(1, 1000))
                    elif "bool" in param_type:
                        constructor_args.append(random.choice([True, False]))
                    elif "string" in param_type:
                        constructor_args.append(f"Test{random.randint(1, 100)}")
                    elif "address" in param_type:
                        constructor_args.append("0x" + "".join(random.choices("abcdef0123456789", k=40)))
        return constructor_args

    def deploy_contract(self):
        """Deploys the contract to Ganache with auto-generated constructor arguments."""
        try:
            function_bytecodes, abi = self.get_function_bytecodes()
            if not function_bytecodes or not abi:
                print("[ERROR] No bytecode or ABI available for deployment!")
                return None

            runtime_bytecode = next(iter(function_bytecodes.values()), None)
            if not runtime_bytecode.startswith("0x"):
                runtime_bytecode = "0x" + runtime_bytecode

            w3 = Web3(Web3.HTTPProvider("http://127.0.0.1:7545"))
            account = w3.eth.accounts[0]

            constructor_args = self.get_constructor_args(abi)

            contract = w3.eth.contract(abi=abi, bytecode=runtime_bytecode)
            tx_hash = contract.constructor(*constructor_args).transact({"from": account})
            receipt = w3.eth.wait_for_transaction_receipt(tx_hash)

            return receipt.contractAddress
        except Exception as e:
            print(f"[ERROR] Contract deployment failed: {e}")
            return None



    def run_symbolic_execution(self):
        """Runs Mythril symbolic execution on the deployed contract."""
        try:
            output = subprocess.run(["myth", "analyze", self.contract_pth], capture_output=True, text=True)
            return output.stdout
        except Exception as e:
            print(f"[ERROR] Symbolic execution failed: {e}")
            return None


    def compare_with(self, other_cfg):
        """Compares two contracts and generates a comprehensive comparison report."""
        report = {
            "matching": [],
            "different_cfg": [],
            "different_ast": [],
            "functionally_equivalent": [],
            "bytecode_identical": [],
            "only_in_self": [],
            "only_in_other": []
        }

        self_functions = set(self.functions_cfg.keys())
        other_functions = set(other_cfg.functions_cfg.keys())

        common_functions = self_functions & other_functions

        for function in common_functions:
            if self.functions_ast[function] == other_cfg.functions_ast[function]:
                report["matching"].append(function)
            elif self.functions_cfg[function] == other_cfg.functions_cfg[function]:
                report["different_ast"].append(function)
            else:
                report["different_cfg"].append(function)

        # Function-level bytecode comparison
        function_bytecode_mismatch = False
        for function in common_functions:
            bytecode_self = self.functions_bytecode.get(self.name, "")
            bytecode_other = other_cfg.functions_bytecode.get(other_cfg.name, "")

            if bytecode_self != bytecode_other:
                print(f"[DEBUG] Bytecode mismatch detected for function: {function}")
                print(f"[DEBUG] Bytecode (self): {bytecode_self[:60]}...")
                print(f"[DEBUG] Bytecode (other): {bytecode_other[:60]}...")
                function_bytecode_mismatch = True

        if function_bytecode_mismatch:
            report["bytecode_identical"].append("Function-level bytecode is different!")
        else:
            report["bytecode_identical"].append("Function-level bytecode matches!")

        report["only_in_self"] = list(self_functions - other_functions)
        report["only_in_other"] = list(other_functions - self_functions)

        return report






if __name__ == "__main__":
    contract1_path = pathlib.Path("contracts/sample1.sol")
    contract2_path = pathlib.Path("contracts/sample2.sol")

    cfg1 = CFG(contract1_path)
    cfg2 = CFG(contract2_path)

    comparison_result = cfg1.compare_with(cfg2)

    print("\n=== FUNCTION COMPARISON REPORT ===")
    for key, value in comparison_result.items():
        print(f"{key.upper()}: {value}")
