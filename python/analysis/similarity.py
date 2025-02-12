#! /usr/bin/env python3

import os
import json
import pathlib
import sys
sys.path.append(str(pathlib.Path(__file__).parent.parent))

from scripts.contract import Contract


class Similarity:
    def __init__(self, contract_a: Contract, contract_b: Contract):
        self.contract_a = contract_a
        self.contract_b = contract_b

    def check_contract_syntax_identicality(self):
        return self.contract_a.all_formats == self.contract_b.all_formats
    
    
    def check_contract_semantic_identicality(self):
        # get functions and variables from ast
        for function in self.contract_a.all_formats["ast"]["functions"]:
            print(function)
        

    


if __name__ == "__main__":
    contract_a = Contract("sample1.sol", pathlib.Path(__file__).parent.parent.parent / "contracts" / "sample1.sol")
    contract_b = Contract("sample2.sol", pathlib.Path(__file__).parent.parent.parent / "contracts" / "sample2.sol")
    similarity = Similarity(contract_a, contract_b)
    print(similarity.check_contract_syntax_identicality())
    print(similarity.check_contract_semantic_identicality())
