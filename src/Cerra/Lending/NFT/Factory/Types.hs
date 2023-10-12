{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Lending.NFT.Factory.Types
  ( FactoryParams (..),
  )
where

import Plutus.V2.Ledger.Api (ValidatorHash, CurrencySymbol)
import qualified PlutusTx
import qualified Prelude as Haskell

data FactoryParams = FactoryParams
  { fValidatorHash :: ValidatorHash,
    fBorrowerSymbol :: CurrencySymbol,
    fLenderSymbol :: CurrencySymbol
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''FactoryParams [('FactoryParams, 0)]
PlutusTx.makeLift ''FactoryParams
