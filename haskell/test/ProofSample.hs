{-# LANGUAGE ScopedTypeVariables #-}

module ProofSample where
import Data.SBV


test1 :: IO ()
test1 = do
    result <- prove $ \(n :: SInteger) -> (n .>= 0) .=> (factorialRecursive n .== factorialIterative n)
    print result





-- Recursive factorial function
factorialRecursive :: SInteger -> SInteger
factorialRecursive n = ite (n .== 0) 1 (n * factorialRecursive (n - 1))

-- Iterative factorial using symbolic loops
factorialIterative :: SInteger -> SInteger
factorialIterative n = go 1 1
  where
    go acc i = ite (i .> n) acc (go (acc * i) (i + 1))