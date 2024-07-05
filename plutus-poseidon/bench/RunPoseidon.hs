{-# LANGUAGE OverloadedStrings #-}

module RunPoseidon (
    runPoseidon,
) where

import GHC.ByteOrder (ByteOrder (..))
import Plutus.Crypto.BlsUtils (mkScalar)
import PlutusTx.Builtins (byteStringToInteger)
import PlutusTx.Prelude (toBuiltin)
import Scripts (listOfSizedByteStrings, poseidonScript)

import PlutusBenchmark.Common (TestSize (TestSize), printHeader, printSizeStatistics)

import System.IO (Handle)
import Text.Printf (hPrintf)

printCostsPoseidon :: Handle -> Integer -> IO ()
printCostsPoseidon h n =
    let script = poseidonScript . map (mkScalar . byteStringToInteger BigEndian . toBuiltin) $ listOfSizedByteStrings n 31
     in printSizeStatistics h (TestSize n) script

runPoseidon :: Handle -> IO ()
runPoseidon h = do
    hPrintf h "\n\n"

    hPrintf h "hash n poseidon inputs (size 31 bytes)\n\n"
    printHeader h
    mapM_ (printCostsPoseidon h) [2, 3, 4, 5, 6]
    hPrintf h "\n\n"
