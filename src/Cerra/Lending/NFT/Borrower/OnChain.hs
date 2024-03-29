{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Cerra.Lending.NFT.Borrower.OnChain
  ( mkBorrowerSymbol,
    mkBorrowerScript
  )
where

import Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedMintingPolicy)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)

import qualified Plutonomy

import Plutus.V2.Ledger.Api
  ( TxInfo
      ( txInfoInputs,
        txInfoMint
      ),
    Script,
    TxInInfo,
    txInInfoResolved,
    txOutValue,
    scriptContextTxInfo,
    CurrencySymbol,
    MintingPolicy,
    unMintingPolicyScript,
    TokenName,
    TxOutRef (TxOutRef),
    TxOut,
    Address,
    txInfoOutputs,
    txInfoValidRange,
    POSIXTime (..)
  )
import Plutus.V2.Ledger.Contexts
  ( ownCurrencySymbol,
    spendsOutput,
    valueSpent,
    ScriptContext
  )
import Cerra.Lending.Contract.Lending.Types
  ( LendingDatum
      ( scBorrowerNFT,
        scLenderNFT,
        scOracleAddressLoan,
        scOracleAddressCollateral,
        scLoanAsset,
        scLoanAmount,
        scCollateralAsset,
        scCollateralAmount,
        scLoanStartTime,
        scLoanLength,
        scInterestPerSecond
      )
  )
import Cerra.Lending.NFT.Borrower.Types (BorrowerParams (..))
import Cerra.Lending.Utils.Utils
  ( getCS,
    getTN,
    mkNftTokenName,
    getContractInput,
    getContractOutput,
    getContinuingContractOutput,
    assetAmount,
    lovelaceAmount,
    assetLength,
    factoryNFT,
    validateFee,
    getLowerBound,
    getUpperBound,
    fromJustCustom,
    isUnity
  )
import Cerra.Lending.Utils.Settings
  ( cerraAssetClass,
    treasuryAddress
  )
import Ledger.Value
  ( assetClass,
    flattenValue,
    AssetClass,
    unAssetClass,
    assetClassValueOf
  )
import Cerra.Lending.Utils.OnChainUtils
  ( mustFindScriptDatum,
    scriptDatumExists
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( BuiltinData,
    length,
    isNothing,
    Bool(..),
    ($),
    (&&),
    (==),
    (/=),
    (<=),
    Bool,
    Integer,
    (+),
    (-),
    snd
  )

import PlutusTx.Builtins (divideInteger, multiplyInteger)

import Cerra.Lending.Utils.Debug (debugError)

{-# INLINEABLE mkBorrowerScript #-}
mkBorrowerScript :: Script
mkBorrowerScript = unMintingPolicyScript originalBorrowerPolicy

{-# INLINABLE originalBorrowerPolicyPlutonomy #-}
originalBorrowerPolicyPlutonomy :: Plutonomy.MintingPolicy
originalBorrowerPolicyPlutonomy = Plutonomy.mkMintingPolicyScript ($$(PlutusTx.compile [|| \param' -> mkUntypedMintingPolicy $ mkBorrowerValidator param' ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
  where
    params =
      BorrowerParams
        { bTreasuryAddress = treasuryAddress,
          bCerraAssetClass = cerraAssetClass
        }

{-# INLINEABLE originalBorrowerPolicy #-}
originalBorrowerPolicy :: MintingPolicy
originalBorrowerPolicy =
  Plutonomy.optimizeUPLC $
    Plutonomy.mintingPolicyToPlutus originalBorrowerPolicyPlutonomy

{-# INLINEABLE mkBorrowerSymbol #-}
mkBorrowerSymbol :: CurrencySymbol
mkBorrowerSymbol = scriptCurrencySymbol originalBorrowerPolicy

{-# INLINEABLE mkBorrowerValidator #-}
mkBorrowerValidator :: BorrowerParams -> BuiltinData -> ScriptContext -> Bool
mkBorrowerValidator BorrowerParams { bTreasuryAddress, bCerraAssetClass } rawRedeemer context =
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

    in if isUnity mintValue borrowerNFT && spendsOutput info refHash refIdx then
            if contractInputExists then validateMintAccept borrowerTokenName info
            else validateMintInit borrowerTokenName bTreasuryAddress bCerraAssetClass info
       else if assetClassValueOf mintValue ownAssetClass == -1 then
            if contractOutputExists then validateBurnRepay (snd $ unAssetClass $ ownAssetClass) info
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

        scriptOutput = getContinuingContractOutput info contractInput
        !outVal = txOutValue scriptOutput
        outValFlatten = flattenValue outVal

        !positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
        borrowerNFTIn             = scBorrowerNFT positionDatumIn
        lenderNFTIn               = scLenderNFT positionDatumIn
        oracleAddressLoanIn       = scOracleAddressLoan positionDatumIn
        oracleAddressCollateralIn = scOracleAddressCollateral positionDatumIn
        loanAssetIn               = scLoanAsset positionDatumIn
        loanAmountIn              = scLoanAmount positionDatumIn
        collateralAssetIn         = scCollateralAsset positionDatumIn
        collateralAmountIn        = scCollateralAmount positionDatumIn
        loanStartTimeIn           = scLoanStartTime positionDatumIn
        loanLengthIn              = scLoanLength positionDatumIn
        interestPerSecondIn       = scInterestPerSecond positionDatumIn

        !positionDatumOut = mustFindScriptDatum @LendingDatum scriptOutput info
        borrowerNFTOut             = scBorrowerNFT positionDatumOut
        lenderNFTOut               = scLenderNFT positionDatumOut
        oracleAddressLoanOut       = scOracleAddressLoan positionDatumOut
        oracleAddressCollateralOut = scOracleAddressCollateral positionDatumOut
        loanAssetOut               = scLoanAsset positionDatumOut
        loanAmountOut              = scLoanAmount positionDatumOut
        collateralAssetOut         = scCollateralAsset positionDatumOut
        collateralAmountOut        = scCollateralAmount positionDatumOut
        loanStartTimeOut           = scLoanStartTime positionDatumOut
        loanLengthOut              = scLoanLength positionDatumOut
        interestPerSecondOut       = scInterestPerSecond positionDatumOut

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
         && lovelaceInInput == 5_000_000
         && outputLength == 3
         && collateralInOutput == collateralAmountOut
         && lovelaceInOutput == 5_000_000
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
        borrowerNFT       = scBorrowerNFT positionDatum
        lenderNFT         = scLenderNFT positionDatum
        loanAsset         = scLoanAsset positionDatum
        collateralAsset   = scCollateralAsset positionDatum
        collateralAmount  = scCollateralAmount positionDatum
        loanStartTime     = scLoanStartTime positionDatum

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
           && lovelaceInOutput == 5_000_000
           &&
           (
             if validateFee bTreasuryAddress cerraInInput info then True
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

        scriptOutput = getContinuingContractOutput info contractInput
        !outVal = txOutValue scriptOutput
        outValFlatten = flattenValue outVal

        !positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
        borrowerNFTIn             = scBorrowerNFT positionDatumIn
        lenderNFTIn               = scLenderNFT positionDatumIn
        oracleAddressLoanIn       = scOracleAddressLoan positionDatumIn
        oracleAddressCollateralIn = scOracleAddressCollateral positionDatumIn
        loanAssetIn               = scLoanAsset positionDatumIn
        loanAmountIn              = scLoanAmount positionDatumIn
        collateralAssetIn         = scCollateralAsset positionDatumIn
        collateralAmountIn        = scCollateralAmount positionDatumIn
        loanStartTimeIn           = scLoanStartTime positionDatumIn
        loanLengthIn              = scLoanLength positionDatumIn
        interestPerSecondIn       = scInterestPerSecond positionDatumIn

        !positionDatumOut = mustFindScriptDatum @LendingDatum scriptOutput info
        borrowerNFTOut             = scBorrowerNFT positionDatumOut
        lenderNFTOut               = scLenderNFT positionDatumOut
        oracleAddressLoanOut       = scOracleAddressLoan positionDatumOut
        oracleAddressCollateralOut = scOracleAddressCollateral positionDatumOut
        loanAssetOut               = scLoanAsset positionDatumOut
        loanAmountOut              = scLoanAmount positionDatumOut
        collateralAssetOut         = scCollateralAsset positionDatumOut
        collateralAmountOut        = scCollateralAmount positionDatumOut
        loanStartTimeOut           = scLoanStartTime positionDatumOut
        loanLengthOut              = scLoanLength positionDatumOut
        interestPerSecondOut       = scInterestPerSecond positionDatumOut

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
         && lovelaceInInput == 5_000_000
         && outputLength == 3
         && loanInOutput == (loanAmountOut + interest)
         && lovelaceInOutput == 5_000_000
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
        borrowerNFTIn       = scBorrowerNFT positionDatumIn
        lenderNFTIn         = scLenderNFT positionDatumIn
        loanAssetIn         = scLoanAsset positionDatumIn
        collateralAssetIn   = scCollateralAsset positionDatumIn
        collateralAmountIn  = scCollateralAmount positionDatumIn
        loanStartTimeIn     = scLoanStartTime positionDatumIn

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
           && lovelaceInInput == 5_000_000
         then True
         else False
