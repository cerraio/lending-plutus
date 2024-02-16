{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Cerra.Lending.Oracle.Orcfax.OnChain
  ( getOrcfaxPrice,
    isOrcfaxSupported,
  )
where

import Cerra.Lending.Oracle.Orcfax.Types (OrcfaxOracleDatum (..))
import Cerra.Lending.Utils.Debug (debugError)
import Cerra.Lending.Utils.OnChainUtils (mustFindScriptDatum)
import Cerra.Lending.Utils.Utils (adaCoin, fromJustCustom, getLowerBound, getUpperBound)
import Plutus.Script.Utils.Value (AssetClass)
import Plutus.V2.Ledger.Api (BuiltinByteString, POSIXTime, TxInfo, TxOut, txInfoValidRange)
import qualified PlutusTx
import PlutusTx.AssocMap as Map
import PlutusTx.Builtins (addInteger, divideInteger, multiplyInteger, subtractInteger)
import qualified PlutusTx.Builtins.Internal as Internals
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinData,
    BuiltinString,
    Integer,
    fst,
    otherwise,
    snd,
    (!!),
    ($),
    (&&),
    (<=),
    (==),
    (>=),
  )

{-# INLINEABLE defaultPrecision #-}
defaultPrecision :: Integer
defaultPrecision = 12

{-# INLINEABLE lovelacePrecision #-}
lovelacePrecision :: Integer
lovelacePrecision = 6

{-# INLINEABLE maxSignedInt64 #-}
maxSignedInt64 :: Integer
maxSignedInt64 = 18446744073709551616

{-# INLINEABLE power10 #-}
power10 :: Integer -> Integer
power10 n
  | n == 0 = 1
  | otherwise = multiplyInteger 10 (power10 (subtractInteger n 1))

{-# INLINEABLE adjustPrice #-}
adjustPrice :: Integer -> Integer -> Integer
adjustPrice x n = case ((addInteger n lovelacePrecision) >= defaultPrecision) of
  True -> divideInteger x (power10 (subtractInteger (addInteger n lovelacePrecision) defaultPrecision))
  False -> multiplyInteger x (power10 (subtractInteger defaultPrecision (addInteger n lovelacePrecision)))

{-# INLINEABLE isOrcfaxSupported #-}
isOrcfaxSupported :: (Integer, (AssetClass, BuiltinString)) -> AssetClass -> Bool
isOrcfaxSupported oracleData asset =
  if snd (snd oracleData) == "orcfax"
    then asset == adaCoin && asset == fst (snd oracleData)
    else True

{-# INLINEABLE getOrcfaxPrice #-}
getOrcfaxPrice :: TxInfo -> TxOut -> (Integer, (AssetClass, BuiltinString))
getOrcfaxPrice info oracleInput =
  let oracleDatum = mustFindScriptDatum @OrcfaxOracleDatum oracleInput info
      oracleData = oxData oracleDatum

      priceName = fromJustCustom $ Map.lookup "name" oracleData
      priceValue = fromJustCustom $ Map.lookup "value" oracleData

      priceData :: BuiltinData
      priceData = (PlutusTx.unsafeFromBuiltinData priceValue) !! 0

      pricePair :: Internals.BuiltinList BuiltinData
      pricePair = Internals.snd (Internals.unsafeDataAsConstr priceData)

      priceDataOne :: Integer
      priceDataOne = PlutusTx.unsafeFromBuiltinData (Internals.head pricePair)

      priceDataTwo :: Integer
      priceDataTwo = PlutusTx.unsafeFromBuiltinData (Internals.head (Internals.tail pricePair))

      precision :: Integer
      precision = subtractInteger maxSignedInt64 priceDataTwo

      adjustedPrice = adjustPrice priceDataOne precision

      valueReference = fromJustCustom $ Map.lookup "valueReference" oracleData
      additionalData :: [Map BuiltinByteString BuiltinData]
      additionalData = PlutusTx.unsafeFromBuiltinData valueReference

      validFromName = fromJustCustom $ Map.lookup "name" (additionalData !! 0)
      validFrom :: POSIXTime
      validFrom = PlutusTx.unsafeFromBuiltinData (fromJustCustom $ Map.lookup "value" (additionalData !! 0))

      lowerBound = getLowerBound (txInfoValidRange info)

      validToName = fromJustCustom $ Map.lookup "name" (additionalData !! 1)
      validTo :: POSIXTime
      validTo = PlutusTx.unsafeFromBuiltinData (fromJustCustom $ Map.lookup "value" (additionalData !! 1))

      upperBound = getUpperBound (txInfoValidRange info)
   in if Internals.decodeUtf8 (Internals.unsafeDataAsB priceName) == "ADA-USD|USD-ADA"
        && Internals.decodeUtf8 (Internals.unsafeDataAsB validFromName) == "validFrom"
        && Internals.decodeUtf8 (Internals.unsafeDataAsB validToName) == "validThrough"
        && lowerBound >= validFrom
        && upperBound <= validTo
        then (adjustedPrice, (adaCoin, "orcfax"))
        else debugError "E7" True
