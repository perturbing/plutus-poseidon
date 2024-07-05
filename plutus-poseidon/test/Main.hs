module Main where

import Plutus.Crypto.BlsUtils
import Plutus.Crypto.Poseidon
import Plutus.Crypto.Poseidon.Constants as Constants

test1 :: Scalar
test1 = mkScalar 17145137698268910157402433918973133955441029919337786522254485807036634975271

test2 :: Scalar
test2 = mkScalar 48687770021716347554360787625135957024624585905005633731415083596211065326696

main :: IO ()
main = do
    print $ poseidon [test2, test1]
    print $ poseidonFixedSize (Constants.c !! 1) (Constants.m !! 1) test2 test1
