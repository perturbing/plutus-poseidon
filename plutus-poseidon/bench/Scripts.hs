{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Scripts (
    poseidonScript,
    poseidonFixedSizeScript,
    listOfSizedByteStrings,
) where

import PlutusTx (compile, getPlcNoAnn, liftCodeDef, unsafeApplyCode)
import PlutusTx.Prelude (Integer, ($), (.))

import Plutus.Crypto.BlsUtils (Scalar)
import Plutus.Crypto.Poseidon (poseidon, poseidonFixedSize)
import PlutusCore (DefaultFun, DefaultUni)
import qualified UntypedPlutusCore as UPLC

import Data.ByteString (ByteString)
import qualified Hedgehog.Internal.Gen as G
import qualified Hedgehog.Internal.Range as R
import System.IO.Unsafe (unsafePerformIO)
import qualified Prelude as Haskell

poseidonScript :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
poseidonScript xs =
    getPlcNoAnn
        $ $$(compile [||poseidon||])
        `unsafeApplyCode` liftCodeDef xs

poseidonFixedSizeScript :: [Scalar] -> [[Scalar]] -> Scalar -> Scalar -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
poseidonFixedSizeScript c m left right =
    getPlcNoAnn
        $ $$(compile [||poseidonFixedSize||])
        `unsafeApplyCode` liftCodeDef c
        `unsafeApplyCode` liftCodeDef m
        `unsafeApplyCode` liftCodeDef left
        `unsafeApplyCode` liftCodeDef right

{-# NOINLINE listOfSizedByteStrings #-}
listOfSizedByteStrings :: Integer -> Integer -> [ByteString]
listOfSizedByteStrings n l =
    unsafePerformIO
        . G.sample
        $ G.list
            (R.singleton $ Haskell.fromIntegral n)
            (G.bytes (R.singleton $ Haskell.fromIntegral l))
