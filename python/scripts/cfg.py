import subprocess
import os
import pathlib
from contract import Contract
from slither.slither import Slither
from solc_select import solc_select



class CFG:

    def __init__(self, contract_pth: pathlib.Path):
        self.name = contract_pth.name.split(".")[0]
        version = Contract.get_contract_version(contract_pth)
        solc_select.switch_global_version(version, always_install=True)
        self.sl = Slither(str(contract_pth))

    

    def export_functions(self, output_dir: pathlib.Path):
        pth = output_dir / f"{self.name}"
        if not pth.exists():
            pth.mkdir(parents=True, exist_ok=True)
        return self.sl.print_functions(str(pth))
    
    def metrics(self):
        for contract in self.sl.contracts:
            print(contract.name)
            for function in contract.functions:
                print(function.name)
                for node in function.nodes:
                    if node.expression:
                        print(node.expression)
    
    @staticmethod
    def clear_cfg(cfg_dir: pathlib.Path):
        for file in cfg_dir.glob("*.dot"):
            file.unlink()



if __name__ == "__main__":
    a = CFG(pathlib.Path(__file__).parent.parent.parent / "contracts" / "sample1.sol")
    # a.export_functions(pathlib.Path(__file__).parent.parent.parent / "cfg")
    a.metrics()