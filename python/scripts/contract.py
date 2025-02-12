#!/usr/bin/env python3

import pathlib
import re
import solcx

class Contract:
    def __init__(self, name, source):
        self.name = name
        self.source = source
        if isinstance(source, pathlib.Path):
            self.source = Contract.read_contract(source)
        parsed = self.parse()
        contract_key = f"<stdin>:{self.name.split('.')[0].capitalize()}"
        self.all_formats = parsed[contract_key]
        self.ast = self.all_formats["ast"]

    def __str__(self):
        return f"Contract(name={self.name}, source={self.source}\n \
                {self.all_formats})"
    
    def __repr__(self):
        return self.__str__()
    
    def parse(self):
        # get version from source
        version = Contract.get_contract_version(self.source)
        if version not in solcx.get_installed_solc_versions():
            print(f"Version {version} not installed, installing...")
            solcx.install_solc(version)
        return solcx.compile_source(self.source,
                                    output_values=["abi", "bin", "bin-runtime", "devdoc", "userdoc", "metadata", "ast"],
                                    solc_version=version)
    
    def compare_ast(self, other_contract):
        """Compare ASTs of two contracts"""
        return self.ast == other_contract.ast
    
    def explore_ast(self):
        """Explore AST of the contract"""
        functions = []
        variables = []
        def recursive_explore(node, level=0):
            nonlocal functions, variables
            if isinstance(node, dict):
                # print("  " * level + node.get("nodeType", ""))
                if "FunctionDefinition" in node.get("nodeType", ""):
                    functions.append(node.get("name", ""))
                if "VariableDeclaration" in node.get("nodeType", ""):
                    variables.append(node.get("name", ""))

                for _, child in node.items():
                    if isinstance(child, (dict, list)):
                        recursive_explore(child, level + 1)
            elif isinstance(node, list):
                for child in node:
                    recursive_explore(child, level + 1)
        recursive_explore(self.ast)
        return functions, variables
    
    @staticmethod
    def get_contract_version(path: pathlib.Path, source: str = None):
        """Get the version of the contract"""
        src = source if source is not None else Contract.read_contract(path)
        version = re.search(r"pragma solidity \^?(\d+\.\d+\.\d+);", src).group(1)
        return version
    
    @staticmethod
    def read_contract(path: pathlib.Path):
        with open(path, "r") as file:
            return file.read()

def main():
    contracts_dir = pathlib.Path(__file__).parent.parent.parent / "contracts"
    a = Contract("sample1.sol", contracts_dir / "sample1.sol")
    print(a)
    functions, variables = a.explore_ast()
    print(functions)
    print(variables)

if __name__ == "__main__":
    main()