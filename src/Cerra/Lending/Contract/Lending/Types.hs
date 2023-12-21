{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

import Plutus.V2.Ledger.Api (Address, POSIXTime, TokenName, CurrencySymbol)
import Ledger (AssetClass)
import PlutusTx.Prelude
  ( Maybe(..),
    Integer
  )
import qualified PlutusTx
import qualified Prelude as Haskell

data LendingDatum = LendingDatum
  { scBorrowerNFT               :: Maybe TokenName,
    scLenderNFT                 :: Maybe TokenName,
    scOracleAddressLoan         :: Address,
    scOracleAddressCollateral   :: Address,
    scLoanAsset                 :: AssetClass,
    scLoanAmount                :: Integer,
    scCollateralAsset           :: AssetClass,
    scCollateralAmount          :: Integer,
    scLoanStartTime             :: Maybe POSIXTime,
    scLoanLength                :: Integer,
    scInterestPerSecond         :: Integer -- need to divide by 10^12, this way getting 12 decimal precision
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''LendingDatum [('LendingDatum, 0)]
PlutusTx.makeLift ''LendingDatum

data LendingRedeemer
  = ApplyRedeem
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
