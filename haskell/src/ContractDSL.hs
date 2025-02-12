module ContractDSL where

-- Define a basic DSL for Solidity contracts.
data Contract = Contract {
    name :: String,
    functions :: [String]
} deriving (Show, Eq)
