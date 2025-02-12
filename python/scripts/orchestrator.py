#!/usr/bin/env python3

import subprocess

def run_analysis():
    print("Starting contract analysis...")
    subprocess.run(["python3", "../analysis/similarity.py"])

if __name__ == '__main__':
    run_analysis()
