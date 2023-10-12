{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Cerra.Lending.Contract.Lending.OnChain
  ( mkLendingScript,
    lendingValidatorHash
  )
where

import qualified Plutonomy

import Plutus.Script.Utils.V2.Typed.Scripts.Validators (mkUntypedValidator)
import Plutus.Script.Utils.V2.Scripts (validatorHash)
import Plutus.V2.Ledger.Contexts
  ( TxInfo
    ( txInfoMint
    ),
    ScriptContext (scriptContextTxInfo),
    txInfoValidRange,
    valueSpent
  )
import Plutus.V2.Ledger.Api
  ( Script,
    unValidatorScript,
    Validator (..),
    TxOut (..),
    ValidatorHash,
    ValidatorHash (..),
    POSIXTime (..)
  )
import Cerra.Lending.Contract.Lending.Types
  ( LendingDatum
      ( LendingDatum,
        scBorrowerNFT,
        scLenderNFT,
        scOracleAddress,
        scLoanAsset,
        scLoanAmount,
        scCollateralAsset,
        scCollateralAmount,
        scLoanStartTime,
        scLoanLength,
        scInterestPerSecond
      ),
    LendingParams (..),
    LendingRedeemer (..),
  )
import Cerra.Lending.NFT.Borrower.OnChain (mkBorrowerSymbol)
import Cerra.Lending.NFT.Lender.OnChain (mkLenderSymbol)
import Cerra.Lending.Utils.Utils
  ( lendingNFTOf,
    assetAmount,
    assetAmountTwoCurrencies,
    lovelaceAmount,
    assetLength,
    fromJustCustom,
    factoryNFT,
    factoryNFTTwoCurrencies,
    isNFTExists,
    isNFTBurned,
    ownContractInput,
    ownContractOutput,
    getUpperBound
  )
import Cerra.Lending.Utils.OnChainUtils
  ( mustFindScriptDatum,
  )
import Ledger.Value
  ( flattenValue,
    AssetClass (..),
    assetClassValueOf
  )
import PlutusTx.Builtins (multiplyInteger)
import qualified PlutusTx
import PlutusTx.Prelude
  ( BuiltinData,
    length,
    ($),
    (&&),
    (||),
    (+),
    (==),
    (<=),
    snd,
    Bool (..)
  )

{-# INLINEABLE mkLendingScript #-}
mkLendingScript :: Script
mkLendingScript = unValidatorScript originalLendingScript

{-# INLINABLE originalLendingScriptPlutonomy #-}
originalLendingScriptPlutonomy :: Plutonomy.Validator
originalLendingScriptPlutonomy =
  Plutonomy.mkValidatorScript
    ($$(PlutusTx.compile [|| \param' -> mkUntypedValidator $ mkLendingValidator param' ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
      where
        params =
          LendingParams
            { scpBorrowerNFT = mkBorrowerSymbol,
              scpLenderNFT = mkLenderSymbol
            }

{-# INLINEABLE originalLendingScript #-}
originalLendingScript :: Validator
originalLendingScript =
  Plutonomy.optimizeUPLC $
    Plutonomy.validatorToPlutus originalLendingScriptPlutonomy

lendingValidatorHash :: ValidatorHash
lendingValidatorHash = validatorHash originalLendingScript

{-# INLINEABLE mkLendingValidator #-}
mkLendingValidator ::
  LendingParams ->
  BuiltinData ->
  BuiltinData ->
  ScriptContext ->
  Bool
mkLendingValidator pp rawDatum rawRedeemer ctx =
  if endpoint == 0 then
    validateCancel pp ctx
  else if endpoint == 1 then
    validateAccept pp ctx
  else if endpoint == 2 then
    validateTake pp datum ctx
  else if endpoint == 3 then
    validateRepay pp ctx
  else if endpoint == 4 then
    validateCollect pp ctx
  else if endpoint == 5 then
    validateLiquidate pp ctx
  else False
  where
    datum = PlutusTx.unsafeFromBuiltinData @LendingDatum rawDatum
    redeemer = PlutusTx.unsafeFromBuiltinData @LendingRedeemer rawRedeemer
    endpoint = scEndpoint redeemer

{-# INLINEABLE validateCancel #-}
validateCancel :: LendingParams -> ScriptContext -> Bool
validateCancel
  LendingParams
    { scpBorrowerNFT,
      scpLenderNFT
    }
  ctx =
    let info = scriptContextTxInfo ctx
        mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue
    in if isNFTBurned mintValFlatten scpBorrowerNFT
          ||
          isNFTBurned mintValFlatten scpLenderNFT
        then True
        else False

{-# INLINEABLE validateAccept #-}
validateAccept :: LendingParams -> ScriptContext -> Bool
validateAccept
  LendingParams
    { scpBorrowerNFT,
      scpLenderNFT
    }
  ctx =
    let info = scriptContextTxInfo ctx
        mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue
    in if isNFTExists mintValFlatten scpBorrowerNFT
          ||
          isNFTExists mintValFlatten scpLenderNFT
        then True
        else False

{-# INLINEABLE validateRepay #-}
validateRepay :: LendingParams -> ScriptContext -> Bool
validateRepay
  LendingParams
    { scpBorrowerNFT
    }
  ctx =
    let info = scriptContextTxInfo ctx
        mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue
    in if isNFTBurned mintValFlatten scpBorrowerNFT
        then True
        else False

{-# INLINEABLE validateCollect #-}
validateCollect :: LendingParams -> ScriptContext -> Bool
validateCollect
  LendingParams
    { scpLenderNFT
    }
  ctx =
    let info = scriptContextTxInfo ctx
        mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue
    in if isNFTBurned mintValFlatten scpLenderNFT
        then True
        else False

{-# INLINEABLE validateLiquidate #-}
validateLiquidate :: LendingParams -> ScriptContext -> Bool
validateLiquidate
  LendingParams
    { scpLenderNFT
    }
  ctx =
    let info = scriptContextTxInfo ctx
        mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue
    in if isNFTBurned mintValFlatten scpLenderNFT
        then True
        else False

{-# INLINEABLE validateTake #-}
validateTake :: LendingParams -> LendingDatum -> ScriptContext -> Bool
validateTake
  LendingParams
    { scpBorrowerNFT
    }
  LendingDatum
    { scBorrowerNFT       = borrowerNFTIn,
      scLenderNFT         = lenderNFTIn,
      scOracleAddress     = oracleAddressIn,
      scLoanAsset         = loanAssetIn,
      scLoanAmount        = loanAmountIn,
      scCollateralAsset   = collateralAssetIn,
      scCollateralAmount  = collateralAmountIn,
      scLoanStartTime     = loanStartTimeIn,
      scLoanLength        = loanLengthIn,
      scInterestPerSecond = interestPerSecondIn
    }
  ctx =
    let info = scriptContextTxInfo ctx
        mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue

        contractInput = ownContractInput ctx
        !inVal = txOutValue contractInput
        inValFlatten = flattenValue inVal

        contractOutput = ownContractOutput info (txOutAddress contractInput)
        !outVal = txOutValue contractOutput
        outValFlatten = flattenValue outVal

        !positionDatumOut = mustFindScriptDatum @LendingDatum contractOutput info
        borrowerNFTOut       = scBorrowerNFT positionDatumOut
        lenderNFTOut         = scLenderNFT positionDatumOut
        oracleAddressOut     = scOracleAddress positionDatumOut
        loanAssetOut         = scLoanAsset positionDatumOut
        loanAmountOut        = scLoanAmount positionDatumOut
        collateralAssetOut   = scCollateralAsset positionDatumOut
        collateralAmountOut  = scCollateralAmount positionDatumOut
        loanStartTimeOut     = scLoanStartTime positionDatumOut
        loanLengthOut        = scLoanLength positionDatumOut
        interestPerSecondOut = scInterestPerSecond positionDatumOut

        !loanInInput = assetAmountTwoCurrencies loanAssetIn (assetClassValueOf inVal loanAssetIn)
        !nftInInput = factoryNFTTwoCurrencies inValFlatten loanAssetIn collateralAssetIn

        !collateralInOutput = assetAmount collateralAssetOut (assetClassValueOf outVal collateralAssetOut)
        !lovelaceInOutput = lovelaceAmount collateralAssetOut outVal
        !outputLength = assetLength collateralAssetOut (length outValFlatten)
        !nftInOutput = factoryNFT outValFlatten collateralAssetOut

        !borrowerNFTinUserInput = lendingNFTOf (valueSpent info) scpBorrowerNFT

        nowTime :: POSIXTime
        nowTime = getUpperBound (txInfoValidRange info)

        loanEndsAt :: POSIXTime
        loanEndsAt = POSIXTime ((getPOSIXTime (fromJustCustom loanStartTimeIn)) + (multiplyInteger loanLengthIn 1000))

     in if nftInInput == nftInOutput
         && (length mintValFlatten) == 0
         && nowTime <= loanEndsAt
         && snd (unAssetClass borrowerNFTinUserInput) == fromJustCustom borrowerNFTIn
         && fromJustCustom borrowerNFTIn == fromJustCustom borrowerNFTOut
         && fromJustCustom lenderNFTIn == fromJustCustom lenderNFTOut
         && oracleAddressIn == oracleAddressOut
         && loanAssetIn == loanAssetOut
         && loanAmountIn == loanAmountOut
         && collateralAssetIn == collateralAssetOut
         && collateralAmountIn == collateralAmountOut
         && fromJustCustom loanStartTimeIn == fromJustCustom loanStartTimeOut
         && loanLengthIn == loanLengthOut
         && interestPerSecondIn == interestPerSecondOut
         && loanInInput == loanAmountIn
         && outputLength == 3
         && collateralInOutput == collateralAmountOut
         && lovelaceInOutput == 2_000_000
        then True
        else False