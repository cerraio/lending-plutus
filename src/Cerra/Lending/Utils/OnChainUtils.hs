{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Improved Minswap version

module Cerra.Lending.Utils.OnChainUtils
  ( mustFindScriptDatum,
    mustFindScriptDatumRaw,
    scriptDatumExists,
    integerToBS,
    bsToInteger,
    encodeHex
  )
where

import Plutus.V2.Ledger.Api (Datum (Datum), txOutDatum, OutputDatum(..), getDatum)
import Plutus.V2.Ledger.Contexts (TxInfo, TxOut, findDatum)

import qualified PlutusTx
import PlutusTx.IsData.Class (UnsafeFromData)
import PlutusTx.Prelude

minusAsciiCode :: Integer
minusAsciiCode = 45

zeroAsciiCode :: Integer
zeroAsciiCode = 48

{-# INLINEABLE mustFindScriptDatum #-}
mustFindScriptDatum :: (UnsafeFromData d) => TxOut -> TxInfo -> d
mustFindScriptDatum o info = case txOutDatum o of
    OutputDatum dat -> PlutusTx.unsafeFromBuiltinData (getDatum dat)
    OutputDatumHash dh -> case findDatum dh info of
        Just (Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
        _ -> error ()
    NoOutputDatum -> error ()

{-# INLINEABLE mustFindScriptDatumRaw #-}
mustFindScriptDatumRaw :: TxOut -> TxInfo -> Datum
mustFindScriptDatumRaw o info = case txOutDatum o of
    OutputDatum dat -> dat
    OutputDatumHash dh -> case findDatum dh info of
        Just (Datum dat) -> PlutusTx.unsafeFromBuiltinData dat
        _ -> error ()
    NoOutputDatum -> error ()

{-# INLINEABLE scriptDatumExists #-}
scriptDatumExists :: TxOut -> Bool
scriptDatumExists output = case txOutDatum output of
    OutputDatum _ -> True
    OutputDatumHash _ -> True
    NoOutputDatum -> False

-- Convert from an integer to its text representation. Example: 123 => "123"
{-# INLINEABLE integerToBS #-}
integerToBS :: Integer -> BuiltinByteString
integerToBS x
  | x < 0 = consByteString minusAsciiCode $ integerToBS (negate x)
  -- x is single-digit
  | x `quotient` 10 == 0 = digitToBS x
  | otherwise = integerToBS (x `quotient` 10) <> digitToBS (x `remainder` 10)
  where
    digitToBS :: Integer -> BuiltinByteString
    digitToBS d = consByteString (d + zeroAsciiCode) emptyByteString

-- Convert an ASCII string representing an integer to an integer. Error if invalid.
{-# INLINEABLE bsToInteger #-}
bsToInteger :: BuiltinByteString -> Integer
bsToInteger input = go 0 0
  where
    len = lengthOfByteString input

    go :: Integer -> Integer -> Integer
    go idx acc
      | idx == len = acc
      | idx == 0 && byte == minusAsciiCode = negate $ go (idx + 1) acc
      | byte < zeroAsciiCode || byte > zeroAsciiCode + 9 = error ()
      | otherwise = go (idx + 1) (acc * 10 + (byte - zeroAsciiCode))
      where
        byte = indexByteString input idx

-- Convert from a byte string to its hex (base16) representation. Example: [2, 14, 255] => "020eff"
{-# INLINEABLE encodeHex #-}
encodeHex :: BuiltinByteString -> BuiltinByteString
encodeHex input = go 0
  where
    len = lengthOfByteString input

    go :: Integer -> BuiltinByteString
    go i
      | i == len = emptyByteString
      | otherwise =
        consByteString (toChar $ byte `quotient` 16) $
          consByteString (toChar $ byte `remainder` 16) (go $ i + 1)
      where
        byte = indexByteString input i

        toChar :: Integer -> Integer
        toChar x
          -- 48 is ASCII code for '0'
          | x < 10 = x + 48
          -- 97 is ASCII code for 'a'
          -- x - 10 + 97 = x + 87
          | otherwise = x + 87
