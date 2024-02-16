{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use underscore" #-}

module Cerra.Lending.NFT.Borrower.OnChain
  ( mkBorrowerSymbol,
    mkBorrowerScript,
    originalBorrowerPolicy,
  )
where

import Cerra.Lending.Contract.Lending.Types
  ( LendingDatum
      ( scBorrowerNFT,
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
  )
import Cerra.Lending.NFT.Borrower.Types (BorrowerParams (..))
import Cerra.Lending.Utils.Debug (debugError)
import Cerra.Lending.Utils.OnChainUtils
  ( mustFindScriptDatum,
    scriptDatumExists,
  )
import Cerra.Lending.Utils.Settings
  ( cerraAssetClass,
    treasuryAddress,
  )
import Cerra.Lending.Utils.Utils
  ( assetAmount,
    assetLength,
    factoryNFT,
    fromJustCustom,
    getCS,
    getContractInput,
    getContractOutput,
    getLowerBound,
    getTN,
    getUpperBound,
    isUnity,
    lovelaceAmount,
    mkNftTokenName,
    validateFee,
  )
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)
import Plutus.Script.Utils.Value
  ( AssetClass,
    assetClass,
    assetClassValueOf,
    flattenValue,
    unAssetClass,
  )
import Plutus.V2.Ledger.Api
  ( Address,
    CurrencySymbol,
    MintingPolicy,
    POSIXTime (..),
    Script,
    TokenName,
    TxInInfo,
    TxInfo
      ( txInfoInputs,
        txInfoMint
      ),
    TxOut,
    TxOutRef (TxOutRef),
    mkMintingPolicyScript,
    scriptContextTxInfo,
    txInInfoResolved,
    txInfoOutputs,
    txInfoValidRange,
    txOutValue,
    unMintingPolicyScript,
  )
import Plutus.V2.Ledger.Contexts
  ( ScriptContext,
    ownCurrencySymbol,
    spendsOutput,
    valueSpent,
  )
import qualified PlutusTx
import PlutusTx.Builtins (divideInteger, multiplyInteger)
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinData,
    Integer,
    isNothing,
    length,
    snd,
    ($),
    (&&),
    (+),
    (-),
    (/=),
    (<=),
    (==),
  )

{-# INLINEABLE mkBorrowerScript #-}
mkBorrowerScript :: Script
mkBorrowerScript = unMintingPolicyScript originalBorrowerPolicy

{-# INLINEABLE originalBorrowerPolicy #-}
originalBorrowerPolicy :: MintingPolicy
originalBorrowerPolicy = mkMintingPolicyScript ($$(PlutusTx.compile [||\param' -> mkUntypedMintingPolicy $ mkBorrowerValidator param'||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
  where
    params =
      BorrowerParams
        { bTreasuryAddress = treasuryAddress,
          bCerraAssetClass = cerraAssetClass
        }

{-# INLINEABLE mkBorrowerSymbol #-}
mkBorrowerSymbol :: CurrencySymbol
mkBorrowerSymbol = scriptCurrencySymbol originalBorrowerPolicy

{-# INLINEABLE mkBorrowerValidator #-}
mkBorrowerValidator :: BorrowerParams -> BuiltinData -> ScriptContext -> Bool
mkBorrowerValidator BorrowerParams {bTreasuryAddress, bCerraAssetClass} rawRedeemer context =
  let ref@(TxOutRef refHash refIdx) = PlutusTx.unsafeFromBuiltinData @TxOutRef rawRedeemer
      info = scriptContextTxInfo context
      ownSymbol = ownCurrencySymbol context
      mintValue = txInfoMint info

      txInputs :: [TxInInfo]
      !txInputs = txInfoInputs info

      contractInputExists :: Bool
      !contractInputExists = case [i | i <- txInputs, scriptDatumExists (txInInfoResolved i)] of
        [_] -> True
        _ -> False

      txOutputs :: [TxOut]
      txOutputs = txInfoOutputs info

      contractOutputExists :: Bool
      !contractOutputExists = case [o | o <- txOutputs, scriptDatumExists o] of
        [_] -> True
        _ -> False

      ownAssetClass = case [c | c <- (flattenValue mintValue), ownSymbol == getCS c] of
        [o] -> assetClass (getCS o) (getTN o)
        _ -> debugError "E1" True

      borrowerTokenName = mkNftTokenName ref

      borrowerNFT :: AssetClass
      borrowerNFT = assetClass ownSymbol borrowerTokenName
   in if isUnity mintValue borrowerNFT && spendsOutput info refHash refIdx
        then
          if contractInputExists
            then validateMintAccept borrowerTokenName info
            else validateMintInit borrowerTokenName bTreasuryAddress bCerraAssetClass info
        else
          if assetClassValueOf mintValue ownAssetClass == -1
            then
              if contractOutputExists
                then validateBurnRepay (snd $ unAssetClass $ ownAssetClass) info
                else validateBurnClose (snd $ unAssetClass $ ownAssetClass) info
            else False

{-# INLINEABLE validateMintAccept #-}
validateMintAccept :: TokenName -> TxInfo -> Bool
validateMintAccept borrowerTokenName info =
  let mintValue = txInfoMint info
      mintValFlatten = flattenValue mintValue

      contractInput = getContractInput info
      !inVal = txOutValue contractInput
      inValFlatten = flattenValue inVal

      scriptOutput = getContractOutput info
      !outVal = txOutValue scriptOutput
      outValFlatten = flattenValue outVal

      !positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
      borrowerNFTIn = scBorrowerNFT positionDatumIn
      lenderNFTIn = scLenderNFT positionDatumIn
      oracleAddressLoanIn = scOracleAddressLoan positionDatumIn
      oracleAddressCollateralIn = scOracleAddressCollateral positionDatumIn
      loanAssetIn = scLoanAsset positionDatumIn
      loanAmountIn = scLoanAmount positionDatumIn
      collateralAssetIn = scCollateralAsset positionDatumIn
      collateralAmountIn = scCollateralAmount positionDatumIn
      loanStartTimeIn = scLoanStartTime positionDatumIn
      loanLengthIn = scLoanLength positionDatumIn
      interestPerSecondIn = scInterestPerSecond positionDatumIn

      !positionDatumOut = mustFindScriptDatum @LendingDatum scriptOutput info
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

      !loanInInput = assetAmount loanAssetIn (assetClassValueOf inVal loanAssetIn)
      !lovelaceInInput = lovelaceAmount loanAssetIn inVal
      !inputLength = assetLength loanAssetIn (length inValFlatten)
      !nftInInput = factoryNFT inValFlatten loanAssetIn

      !collateralInOutput = assetAmount collateralAssetOut (assetClassValueOf outVal collateralAssetOut)
      !lovelaceInOutput = lovelaceAmount collateralAssetOut outVal
      !outputLength = assetLength collateralAssetOut (length outValFlatten)
      !nftInOutput = factoryNFT outValFlatten collateralAssetOut
   in if nftInInput == nftInOutput
        && (length mintValFlatten) == 1
        && loanAssetIn /= collateralAssetIn
        && isNothing borrowerNFTIn
        && (fromJustCustom borrowerNFTOut) == borrowerTokenName
        && fromJustCustom lenderNFTIn == fromJustCustom lenderNFTOut
        && oracleAddressLoanIn == oracleAddressLoanOut
        && oracleAddressCollateralIn == oracleAddressCollateralOut
        && loanAssetIn == loanAssetOut
        && loanAmountIn == loanAmountOut
        && collateralAssetIn == collateralAssetOut
        && collateralAmountIn == collateralAmountOut
        && isNothing loanStartTimeIn
        && getLowerBound (txInfoValidRange info) == fromJustCustom loanStartTimeOut
        && loanLengthIn == loanLengthOut
        && interestPerSecondIn == interestPerSecondOut
        && inputLength == 3
        && loanInInput == loanAmountIn
        && lovelaceInInput == 2_000_000
        && outputLength == 3
        && collateralInOutput == collateralAmountOut
        && lovelaceInOutput == 2_000_000
        then True
        else False

{-# INLINEABLE validateMintInit #-}
validateMintInit :: TokenName -> Address -> AssetClass -> TxInfo -> Bool
validateMintInit borrowerTokenName bTreasuryAddress bCerraAssetClass info =
  let mintValue = txInfoMint info
      mintValFlatten = flattenValue mintValue
      valSpent = valueSpent info

      scriptOutput = getContractOutput info
      !outVal = txOutValue scriptOutput
      !outValFlatten = flattenValue outVal

      !positionDatum = mustFindScriptDatum @LendingDatum scriptOutput info
      borrowerNFT = scBorrowerNFT positionDatum
      lenderNFT = scLenderNFT positionDatum
      loanAsset = scLoanAsset positionDatum
      collateralAsset = scCollateralAsset positionDatum
      collateralAmount = scCollateralAmount positionDatum
      loanStartTime = scLoanStartTime positionDatum

      !collateralInOutput = assetAmount collateralAsset (assetClassValueOf outVal collateralAsset)
      !lovelaceInOutput = lovelaceAmount collateralAsset outVal
      !outputLength = assetLength collateralAsset (length outValFlatten)
      !nftInOutput = factoryNFT outValFlatten collateralAsset

      !cerraInInput = assetClassValueOf valSpent bCerraAssetClass
   in if (fromJustCustom borrowerNFT) == borrowerTokenName
        && loanAsset /= collateralAsset
        && isNothing lenderNFT
        && isNothing loanStartTime
        && (length mintValFlatten) == 2
        && outputLength == 3
        && collateralInOutput == collateralAmount
        && assetClassValueOf mintValue nftInOutput == 1
        && lovelaceInOutput == 2_000_000
        && ( if validateFee bTreasuryAddress cerraInInput info
               then True
               else False
           )
        then True
        else False

{-# INLINEABLE validateBurnRepay #-}
validateBurnRepay :: TokenName -> TxInfo -> Bool
validateBurnRepay borrowerTokenName info =
  let mintValue = txInfoMint info
      mintValFlatten = flattenValue mintValue

      contractInput = getContractInput info
      !inVal = txOutValue contractInput
      inValFlatten = flattenValue inVal

      scriptOutput = getContractOutput info
      !outVal = txOutValue scriptOutput
      outValFlatten = flattenValue outVal

      !positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
      borrowerNFTIn = scBorrowerNFT positionDatumIn
      lenderNFTIn = scLenderNFT positionDatumIn
      oracleAddressLoanIn = scOracleAddressLoan positionDatumIn
      oracleAddressCollateralIn = scOracleAddressCollateral positionDatumIn
      loanAssetIn = scLoanAsset positionDatumIn
      loanAmountIn = scLoanAmount positionDatumIn
      collateralAssetIn = scCollateralAsset positionDatumIn
      collateralAmountIn = scCollateralAmount positionDatumIn
      loanStartTimeIn = scLoanStartTime positionDatumIn
      loanLengthIn = scLoanLength positionDatumIn
      interestPerSecondIn = scInterestPerSecond positionDatumIn

      !positionDatumOut = mustFindScriptDatum @LendingDatum scriptOutput info
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

      !collateralInInput = assetAmount collateralAssetIn (assetClassValueOf inVal collateralAssetIn)
      !lovelaceInInput = lovelaceAmount collateralAssetIn inVal
      !inputLength = assetLength collateralAssetIn (length inValFlatten)
      !nftInInput = factoryNFT inValFlatten collateralAssetIn

      !loanInOutput = assetAmount loanAssetOut (assetClassValueOf outVal loanAssetOut)
      !lovelaceInOutput = lovelaceAmount loanAssetOut outVal
      !outputLength = assetLength loanAssetOut (length outValFlatten)
      !nftInOutput = factoryNFT outValFlatten loanAssetOut

      nowTime :: POSIXTime
      nowTime = getUpperBound (txInfoValidRange info)

      loanEndsAt :: POSIXTime
      loanEndsAt = POSIXTime ((getPOSIXTime (fromJustCustom loanStartTimeIn)) + (multiplyInteger loanLengthIn 1000))

      interest :: Integer
      interest = divideInteger (multiplyInteger (multiplyInteger (divideInteger ((getPOSIXTime nowTime) - (getPOSIXTime (fromJustCustom loanStartTimeIn))) 1_000) interestPerSecondIn) loanAmountIn) 1_000_000_000_000
   in if nftInInput == nftInOutput
        && (length mintValFlatten) == 1
        && loanAssetIn /= collateralAssetIn
        && (fromJustCustom borrowerNFTIn) == borrowerTokenName
        && isNothing borrowerNFTOut
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
        && inputLength == 3
        && collateralInInput == collateralAmountIn
        && lovelaceInInput == 2_000_000
        && outputLength == 3
        && loanInOutput == (loanAmountOut + interest)
        && lovelaceInOutput == 2_000_000
        && nowTime <= loanEndsAt
        then True
        else False

{-# INLINEABLE validateBurnClose #-}
validateBurnClose :: TokenName -> TxInfo -> Bool
validateBurnClose borrowerTokenName info =
  let mintValue = txInfoMint info
      mintValFlatten = flattenValue mintValue

      contractInput = getContractInput info
      !inVal = txOutValue contractInput
      inValFlatten = flattenValue inVal

      !positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
      borrowerNFTIn = scBorrowerNFT positionDatumIn
      lenderNFTIn = scLenderNFT positionDatumIn
      loanAssetIn = scLoanAsset positionDatumIn
      collateralAssetIn = scCollateralAsset positionDatumIn
      collateralAmountIn = scCollateralAmount positionDatumIn
      loanStartTimeIn = scLoanStartTime positionDatumIn

      !collateralInInput = assetAmount collateralAssetIn (assetClassValueOf inVal collateralAssetIn)
      !lovelaceInInput = lovelaceAmount collateralAssetIn inVal
      !inputLength = assetLength collateralAssetIn (length inValFlatten)
      !nftInInput = factoryNFT inValFlatten collateralAssetIn
   in if (fromJustCustom borrowerNFTIn) == borrowerTokenName
        && loanAssetIn /= collateralAssetIn
        && isNothing lenderNFTIn
        && isNothing loanStartTimeIn
        && (length mintValFlatten) == 2
        && inputLength == 3
        && collateralInInput == collateralAmountIn
        && assetClassValueOf mintValue nftInInput == -1
        && lovelaceInInput == 2_000_000
        then True
        else False
