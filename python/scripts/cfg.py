import subprocess
import os
import pathlib
from slither.slither import Slither

class CFG:

    def __init__(self, contract_pth: pathlib.Path):
        self.sl = Slither(str(contract_pth))

    

    def get_functions(self):
        return self.sl.print_functions()



if __name__ == "__main__":
    a = CFG(pathlib.Path(__file__).parent.parent.parent / "contracts" / "sample1.sol")

        