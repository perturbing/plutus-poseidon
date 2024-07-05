{-# LANGUAGE OverloadedStrings #-}

module RunPoseidon (
    runPoseidon,
) where

import GHC.ByteOrder (ByteOrder (..))
import Plutus.Crypto.BlsUtils (mkScalar)
import qualified Plutus.Crypto.Poseidon.Constants as Constants
import PlutusTx.Builtins (byteStringToInteger)
import PlutusTx.Prelude (toBuiltin)
import Scripts (listOfSizedByteStrings, poseidonFixedSizeScript, poseidonScript)

import PlutusBenchmark.Common (TestSize (..), printHeader, printSizeStatistics)

import System.IO (Handle)
import Text.Printf (hPrintf)

printCostsPoseidon :: Handle -> Integer -> IO ()
printCostsPoseidon h n =
    let script = poseidonScript . map (mkScalar . byteStringToInteger BigEndian . toBuiltin) $ listOfSizedByteStrings n 31
     in printSizeStatistics h (TestSize n) script

printCostsPoseidonFixedSize :: Handle -> IO ()
printCostsPoseidonFixedSize h =
    let (left : right : xs) = map (mkScalar . byteStringToInteger BigEndian . toBuiltin) $ listOfSizedByteStrings 2 31
        script = poseidonFixedSizeScript (Constants.c !! 1) (Constants.m !! 1) left right
     in printSizeStatistics h NoSize script

runPoseidon :: Handle -> IO ()
runPoseidon h = do
    hPrintf h "\n\n"

    hPrintf h "hash n poseidon inputs (size 31 bytes) \n\n"
    printHeader h
    mapM_ (printCostsPoseidon h) [2, 3, 4, 5, 6]
    hPrintf h "\n\n"

    hPrintf h "Run poseidon hash for only 2 inputs \n\n"
    printHeader h
    printCostsPoseidonFixedSize h
    hPrintf h "\n\n"
