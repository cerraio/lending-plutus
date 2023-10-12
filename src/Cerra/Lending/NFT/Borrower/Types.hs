{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Lending.NFT.Borrower.Types
  ( BorrowerParams (..),
  )
where

import Plutus.V2.Ledger.Api (Address)
import Ledger (AssetClass)
import qualified PlutusTx
import qualified Prelude as Haskell

data BorrowerParams = BorrowerParams
  { bTreasuryAddress :: Address,
    bCerraAssetClass :: AssetClass
  }
  deriving stock (Haskell.Show)

PlutusTx.makeIsDataIndexed ''BorrowerParams [('BorrowerParams, 0)]
PlutusTx.makeLift ''BorrowerParams
