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

module Cerra.Lending.Contract.Lending.Types
  ( LendingDatum (..),
    LendingRedeemer (..),
    LendingParams (..),
  )
where

import Plutus.Script.Utils.Value (AssetClass)
import Plutus.V2.Ledger.Api (Address, CurrencySymbol, POSIXTime, TokenName)
import qualified PlutusTx
import PlutusTx.Prelude
  ( Integer,
    Maybe (..),
  )
import qualified Prelude as Haskell

data LendingDatum = LendingDatum
  { scBorrowerNFT :: Maybe TokenName,
    scLenderNFT :: Maybe TokenName,
    scOracleAddressLoan :: Address,
    scOracleAddressCollateral :: Address,
    scLoanAsset :: AssetClass,
    scLoanAmount :: Integer,
    scCollateralAsset :: AssetClass,
    scCollateralAmount :: Integer,
    scLoanStartTime :: Maybe POSIXTime,
    scLoanLength :: Integer,
    scInterestPerSecond :: Integer -- need to divide by 10^12, this way getting 12 decimal precision
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''LendingDatum [('LendingDatum, 0)]
PlutusTx.makeLift ''LendingDatum

data LendingRedeemer = ApplyRedeem
  { scEndpoint :: Integer
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed
  ''LendingRedeemer
  [('ApplyRedeem, 0)]
PlutusTx.makeLift ''LendingRedeemer

data LendingParams = LendingParams
  { scpBorrowerNFT :: CurrencySymbol,
    scpLenderNFT :: CurrencySymbol
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''LendingParams [('LendingParams, 0)]
PlutusTx.makeLift ''LendingParams
