module Main where

import Verification
import ContractDSL

main :: IO ()
main = do
    let c1 = Contract "Sample" []
        c2 = Contract "Sample" []
    if verifySimilarity c1 c2
       then putStrLn "Test passed!"
       else putStrLn "Test failed!"
