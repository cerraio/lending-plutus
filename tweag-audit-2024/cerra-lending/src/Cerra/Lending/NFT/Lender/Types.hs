{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Lending.NFT.Lender.Types
  ( LenderParams (..),
  )
where

import Plutus.Script.Utils.Value (AssetClass)
import Plutus.V2.Ledger.Api (Address, CurrencySymbol)
import qualified PlutusTx
import qualified Prelude as Haskell

data LenderParams = LenderParams
  { lTreasuryAddress :: Address,
    lCerraAssetClass :: AssetClass,
    lOracleFactoryAssetClass :: AssetClass,
    lOracleFactorySymbolOrcfax :: CurrencySymbol
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''LenderParams [('LenderParams, 0)]
PlutusTx.makeLift ''LenderParams
