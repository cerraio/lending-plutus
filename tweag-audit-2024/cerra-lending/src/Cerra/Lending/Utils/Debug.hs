{-# LANGUAGE TupleSections #-}

-- https://gist.github.com/bwbush/623543faa27c75b9ae0c8c302011b110

module Cerra.Lending.Utils.Debug
  ( debug,
    debugError,
    debugIfFalse,
    debugIfTrue,
    recoverFromData,
    recoverPlutusData,
  )
where

import Codec.Serialise (deserialise)
import qualified Data.ByteString.Base16 as Base16 (decode)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import qualified Data.Text as T (unpack)
import Plutus.V2.Ledger.Api (ToData (..), unsafeFromBuiltinData)
import PlutusTx (Data, FromData (..), fromData)
import PlutusTx.Prelude (BuiltinString, decodeUtf8, encodeUtf8, error, fromBuiltin, trace)
import Prelude hiding (error)

{-# INLINE debug #-}

-- | Write a message and serialised data to the trace log.
debug ::
  ToData a =>
  -- | A message.
  BuiltinString ->
  -- | The data.
  a ->
  -- | The value to be returned.
  b ->
  -- | The vaule returned.
  b
debug message =
  trace
    . decodeUtf8
    . unsafeFromBuiltinData
    . toBuiltinData
    . (encodeUtf8 message,)

{-# INLINE debugIfTrue #-}

-- | Write a message and serialised data to the trace log, if a condition holds.
debugIfTrue ::
  ToData a =>
  -- | The message.
  BuiltinString ->
  -- | The data.
  a ->
  -- | The condition to be returned.
  Bool ->
  -- | The condition.
  Bool
debugIfTrue message x condition =
  condition && debug message x True

{-# INLINE debugIfFalse #-}

-- | Write a message and serialised data to the trace log, if a condition does not hold.
debugIfFalse ::
  ToData a =>
  -- | The message.
  BuiltinString ->
  -- | The data.
  a ->
  -- | The condition to be returned.
  Bool ->
  -- | The condition.
  Bool
debugIfFalse message x condition =
  condition || debug message x False

{-# INLINE debugError #-}

-- | Raise an error after writing a message and serialised data to the trace log.
debugError ::
  ToData a =>
  -- | The message.
  BuiltinString ->
  -- | The data.
  a ->
  -- | The type to be returned.
  b
debugError message x = error $ debug message x ()

-- | Recover serialised debugging data.
recoverPlutusData ::
  -- | The base-16 encoding of the data.
  String ->
  -- | The Plutus data.
  Either String Data
recoverPlutusData raw =
  do
    bytes <- Base16.decode . BS8.pack $ raw
    pure
      . deserialise
      . LBS.fromStrict
      $ bytes

-- | Recover serialised debugging data.
recoverFromData ::
  FromData a =>
  -- | The base-16 encoding of the data.
  String ->
  -- | The original message and data.
  Either String (String, a)
recoverFromData raw =
  do
    raw' <- recoverPlutusData raw
    (message, x) <-
      maybe (Left "Failed decoding Plutus data.") Right $
        fromData raw'
    pure
      ( T.unpack . fromBuiltin . decodeUtf8 $ message,
        x
      )
