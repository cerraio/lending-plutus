{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Lending.Oracle.Cerra.Types
  ( CerraOracleDatum (..)
  )
where

import Plutus.V2.Ledger.Api (POSIXTime)
import Ledger (AssetClass)
import qualified PlutusTx
import qualified Prelude as Haskell
import PlutusTx.Prelude (Integer)

data CerraOracleDatum = CerraOracleDatum
  { oAsset         :: AssetClass,
    oUpdateTime    :: POSIXTime,
    oPrice         :: Integer -- need to divide by 10^12, this way getting 12 decimal precision. Can accurately get price per lovelace
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''CerraOracleDatum [('CerraOracleDatum, 0)]
PlutusTx.makeLift ''CerraOracleDatum
