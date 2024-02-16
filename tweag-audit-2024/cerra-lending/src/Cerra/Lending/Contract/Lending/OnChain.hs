{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use null" #-}
{-# HLINT ignore "Use underscore" #-}

module Cerra.Lending.Contract.Lending.OnChain
  ( 
    lendingValidatorHash,
    lendingValidator,
    Lending,
  )
where

import Cerra.Lending.Contract.Lending.Types
  ( LendingDatum
      ( LendingDatum,
        scBorrowerNFT,
        scCollateralAmount,
        scCollateralAsset,
        scInterestPerSecond,
        scLenderNFT,
        scLoanAmount,
        scLoanAsset,
        scLoanLength,
        scLoanStartTime,
        scOracleAddressCollateral,
        scOracleAddressLoan
      ),
    LendingParams (..),
    LendingRedeemer (..),
  )
import Cerra.Lending.NFT.Borrower.OnChain (mkBorrowerSymbol)
import Cerra.Lending.NFT.Lender.OnChain (mkLenderSymbol)
import Cerra.Lending.Utils.OnChainUtils
  ( mustFindScriptDatum,
  )
import Cerra.Lending.Utils.Utils
  ( assetAmount,
    assetAmountTwoCurrencies,
    assetLength,
    factoryNFT,
    factoryNFTTwoCurrencies,
    fromJustCustom,
    getUpperBound,
    isNFTBurned,
    isNFTExists,
    lendingNFTOf,
    lovelaceAmount,
    ownContractInput,
    ownContractOutput,
  )
import Ledger.Typed.Scripts (validatorHash)
import Plutus.Script.Utils.Typed (TypedValidator, ValidatorTypes (..), mkUntypedValidator)
import Plutus.Script.Utils.V2.Typed.Scripts.Validators (mkTypedValidator)
import Plutus.Script.Utils.Value
  ( AssetClass (..),
    assetClassValueOf,
    flattenValue,
  )
import Plutus.V2.Ledger.Api
  ( POSIXTime (..),
    TxOut (..),
    ValidatorHash (..),
  )
import Plutus.V2.Ledger.Contexts
  ( ScriptContext (scriptContextTxInfo),
    TxInfo
      ( txInfoMint
      ),
    txInfoValidRange,
    valueSpent,
  )
import qualified PlutusTx
import PlutusTx.Builtins (multiplyInteger)
import PlutusTx.Prelude
  ( Bool (..),
    length,
    snd,
    (&&),
    (+),
    (<=),
    (==),
    (||),
  )

data Lending

instance ValidatorTypes Lending where
  type DatumType Lending = LendingDatum
  type RedeemerType Lending = LendingRedeemer

{-# INLINEABLE lendingValidator #-}
lendingValidator :: TypedValidator Lending
lendingValidator =
  mkTypedValidator @Lending
    ( $$( PlutusTx.compile
            [||mkLendingValidator||]
        )
        `PlutusTx.applyCode` PlutusTx.liftCode
          LendingParams
            { scpBorrowerNFT = mkBorrowerSymbol,
              scpLenderNFT = mkLenderSymbol
            }
    )
    $$(PlutusTx.compile [||mkUntypedValidator||])

------------------------------

lendingValidatorHash :: ValidatorHash
lendingValidatorHash = validatorHash lendingValidator

{-# INLINEABLE mkLendingValidator #-}
mkLendingValidator ::
  LendingParams ->
  LendingDatum ->
  LendingRedeemer ->
  ScriptContext ->
  Bool
mkLendingValidator pp datum redeemer ctx =
  if endpoint == 0
    then validateCancel pp ctx
    else
      if endpoint == 1
        then validateAccept pp ctx
        else
          if endpoint == 2
            then validateTake pp datum ctx
            else
              if endpoint == 3
                then validateRepay pp ctx
                else
                  if endpoint == 4
                    then validateCollect pp ctx
                    else
                      if endpoint == 5
                        then validateLiquidate pp ctx
                        else False
  where
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
          || isNFTBurned mintValFlatten scpLenderNFT
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
          || isNFTExists mintValFlatten scpLenderNFT
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
    { scBorrowerNFT = borrowerNFTIn,
      scLenderNFT = lenderNFTIn,
      scOracleAddressLoan = oracleAddressLoanIn,
      scOracleAddressCollateral = oracleAddressCollateralIn,
      scLoanAsset = loanAssetIn,
      scLoanAmount = loanAmountIn,
      scCollateralAsset = collateralAssetIn,
      scCollateralAmount = collateralAmountIn,
      scLoanStartTime = loanStartTimeIn,
      scLoanLength = loanLengthIn,
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
        borrowerNFTOut = scBorrowerNFT positionDatumOut
        lenderNFTOut = scLenderNFT positionDatumOut
        oracleAddressLoanOut = scOracleAddressLoan positionDatumOut
        oracleAddressCollateralOut = scOracleAddressCollateral positionDatumOut
        loanAssetOut = scLoanAsset positionDatumOut
        loanAmountOut = scLoanAmount positionDatumOut
        collateralAssetOut = scCollateralAsset positionDatumOut
        collateralAmountOut = scCollateralAmount positionDatumOut
        loanStartTimeOut = scLoanStartTime positionDatumOut
        loanLengthOut = scLoanLength positionDatumOut
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
          && oracleAddressLoanIn == oracleAddressLoanOut
          && oracleAddressCollateralIn == oracleAddressCollateralOut
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
