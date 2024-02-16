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

module Cerra.Lending.Oracle.Cerra.Types
  ( CerraOracleDatum (..),
  )
where

import Plutus.Script.Utils.Value (AssetClass)
import Plutus.V2.Ledger.Api (POSIXTime)
import qualified PlutusTx
import PlutusTx.Prelude (Integer)
import qualified Prelude as Haskell

data CerraOracleDatum = CerraOracleDatum
  { oAsset :: AssetClass,
    oUpdateTime :: POSIXTime,
    oPrice :: Integer -- need to divide by 10^12, this way getting 12 decimal precision. Can accurately get price per lovelace
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''CerraOracleDatum [('CerraOracleDatum, 0)]
PlutusTx.makeLift ''CerraOracleDatum
