{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}

{-# HLINT ignore "Avoid lambda" #-}

module Plutus.Crypto.Poseidon where

import Plutus.Crypto.BlsUtils (Scalar)
import qualified Plutus.Crypto.Poseidon.Constants as Constants
import PlutusTx.Numeric (
    AdditiveGroup ((-)),
    AdditiveMonoid (zero),
    AdditiveSemigroup ((+)),
    MultiplicativeSemigroup ((*)),
 )
import PlutusTx.Prelude (
    AdditiveGroup ((-)),
    AdditiveMonoid (zero),
    AdditiveSemigroup ((+)),
    Enum (enumFromTo),
    Integer,
    MultiplicativeSemigroup ((*)),
    Ord ((<), (>), (>=)),
    divide,
    error,
    foldl,
    head,
    id,
    length,
    map,
    null,
    otherwise,
    sum,
    (!!),
    ($),
    (.),
    (==),
    (||),
 )

-- | Map with index
{-# INLINEABLE mapI #-}
mapI :: forall a b. (Integer -> a -> b) -> [a] -> [b]
mapI f = go 0
  where
    go :: Integer -> [a] -> [b]
    go _ [] = []
    go n (x : xs) = f n x : go (n + 1) xs

-- | Add round constants
{-# INLINEABLE arc #-}
arc :: [Scalar] -> Integer -> [Scalar] -> [Scalar]
arc c it = mapI (\i x -> x + c !! (it + i))

-- | Substitution box
{-# INLINEABLE sbox #-}
sbox :: Integer -> Integer -> Integer -> [Scalar] -> [Scalar]
sbox f p r = mapI go
  where
    go n
        | n == 0 = fullSBox
        | r < f `divide` 2 || r >= f `divide` 2 + p = fullSBox
        | otherwise = id
    -- Full S-box x^5
    fullSBox x = x * x * x * x * x

-- | Mix layer
{-# INLINEABLE mix #-}
mix :: [[Scalar]] -> [Scalar] -> [Scalar]
mix m state =
    map
        (\i -> sum (mapI (\j x -> x * (m !! i !! j)) state))
        (enumFromTo 0 (length state - 1))

-- | Poseidon hash function
{-# INLINEABLE poseidon #-}
poseidon :: [Scalar] -> Scalar
poseidon msg =
    if null msg || length msg > 6
        then error ()
        else head $ foldl (\state r -> round r state) initState (enumFromTo 0 (f + p - 1))
  where
    t = length msg + 1
    roundsP = [56, 57, 56, 60, 60, 63, 64, 63]
    f = 8
    p = roundsP !! (t - 2)
    c = Constants.c !! (t - 2)
    m = Constants.m !! (t - 2)
    initState = zero : msg
    round r = mix m . sbox f p r . arc c (r * t)

-- see for source
-- https://github.com/btq-ag/keelung-stdlib/blob/main/src/Hash/Poseidon.hs#L21C1-L21C35

{-# INLINEABLE poseidonFixedSize #-}
poseidonFixedSize :: [Scalar] -> [[Scalar]] -> Scalar -> Scalar -> Scalar
poseidonFixedSize c m left right = head $ foldl (\state r -> round r state) initState (enumFromTo 0 (f + p - 1))
  where
    t = 3
    roundsP = [56, 57, 56, 60, 60, 63, 64, 63]
    f = 8
    p = 57
    initState = zero : [left, right]
    round r = mix m . sbox f p r . arc c (r * t)
