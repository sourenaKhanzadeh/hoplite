module Main where

import System.Environment (getArgs)
import ParseSolidity

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            result <- parseSolidityFile file
            case result of
                Left err -> putStrLn $ "Parse error: " ++ show err
                Right contract -> print contract
        _ -> putStrLn "Usage: runhaskell Main.hs <file.sol>"
