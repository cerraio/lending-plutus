{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Cerra.Lending.Oracle.Cerra.OnChain
  ( getCerraPrice,
  )
where

import Cerra.Lending.Oracle.Cerra.Types (CerraOracleDatum (..))
import Cerra.Lending.Utils.Debug (debugError)
import Cerra.Lending.Utils.OnChainUtils (mustFindScriptDatum)
import Cerra.Lending.Utils.Utils (getUpperBound)
import Plutus.Script.Utils.Value (AssetClass)
import Plutus.V2.Ledger.Api (POSIXTime, TxInfo, TxOut, txInfoValidRange)
import PlutusTx.Prelude
  ( Bool (True),
    BuiltinString,
    Integer,
    (-),
    (>),
  )

{-# INLINEABLE getCerraPrice #-}
getCerraPrice :: TxInfo -> TxOut -> (Integer, (AssetClass, BuiltinString))
getCerraPrice info oracleInput =
  let !oracleDatum = mustFindScriptDatum @CerraOracleDatum oracleInput info
      asset = oAsset oracleDatum
      updateTime = oUpdateTime oracleDatum
      price = oPrice oracleDatum

      nowTime :: POSIXTime
      nowTime = getUpperBound (txInfoValidRange info)

      lastHour = nowTime - 3600000
   in if updateTime > lastHour
        then (price, (asset, "cerra"))
        else debugError "E6" True
