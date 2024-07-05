module Main where

import RunPoseidon (runPoseidon)
import System.IO (stdout)

main :: IO ()
main = do runPoseidon stdout
