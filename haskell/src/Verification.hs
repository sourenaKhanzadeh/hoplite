module Verification where

import ContractDSL

-- A stub function for verifying contract similarities.
verifySimilarity :: Contract -> Contract -> Bool
verifySimilarity c1 c2 = (name c1 == name c2)
