{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Lending.NFT.Lender.Types
  ( LenderParams (..),
    OracleDatum (..)
  )
where

import Plutus.V2.Ledger.Api (Address, POSIXTime)
import Ledger (AssetClass)
import qualified PlutusTx
import qualified Prelude as Haskell
import PlutusTx.Prelude (Integer)

data LenderParams = LenderParams
  { lTreasuryAddress :: Address,
    lCerraAssetClass :: AssetClass,
    lOracleFactoryAssetClass :: AssetClass
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''LenderParams [('LenderParams, 0)]
PlutusTx.makeLift ''LenderParams

data OracleDatum = OracleDatum
  { oAsset         :: AssetClass,
    oUpdateTime    :: POSIXTime,
    oPrice         :: Integer -- need to divide by 10^12, this way getting 12 decimal precision. Can accurately get price per lovelace
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''OracleDatum [('OracleDatum, 0)]
PlutusTx.makeLift ''OracleDatum
