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
        functs_a = set()
        vars_a = set()
        funcs, vars = self.contract_a.explore_ast()
        for func, var in zip(funcs, vars):
            functs_a.add(func)
            vars_a.add(var)
        funcs, vars = self.contract_b.explore_ast()
        print(funcs, vars)
        check_func = True
        check_vars = True
        for func, var in zip(funcs, vars):
            if func not in functs_a:
                check_func = False
            if var not in vars_a:
                check_vars = False

        return check_func, check_vars
        

    


if __name__ == "__main__":
    contract_a = Contract("sample1.sol", pathlib.Path(__file__).parent.parent.parent / "contracts" / "sample1.sol")
    contract_b = Contract("sample2.sol", pathlib.Path(__file__).parent.parent.parent / "contracts" / "sample2.sol")
    similarity = Similarity(contract_a, contract_b)
    print(similarity.check_contract_syntax_identicality())
    print(similarity.check_contract_semantic_identicality())
