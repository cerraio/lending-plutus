{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Lending.Oracle.Orcfax.Types
  ( OrcfaxOracleDatum (..)
  )
where

import Plutus.V2.Ledger.Api (BuiltinByteString, Map)
import qualified PlutusTx
import qualified Prelude as Haskell

data OrcfaxOracleDatum = OrcfaxOracleDatum
  { oxData        :: Map BuiltinByteString PlutusTx.BuiltinData,
    oxUnusedOne   :: BuiltinByteString,
    oxUnusedTwo   :: PlutusTx.BuiltinData,
    oxUnusedThree :: BuiltinByteString
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''OrcfaxOracleDatum [('OrcfaxOracleDatum, 0)]
PlutusTx.makeLift ''OrcfaxOracleDatum
