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

module Cerra.Lending.NFT.Lender.OnChain
  ( mkLenderSymbol,
    mkLenderScript
  )
where

import Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedMintingPolicy)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)

import qualified Plutonomy

import Plutus.V2.Ledger.Api
  ( TxInfo
      ( txInfoInputs,
        txInfoMint,
        txInfoReferenceInputs
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
    Address,
    txInfoValidRange,
    TxOut,
    Value,
    POSIXTime(..),
    getPOSIXTime
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
import Cerra.Lending.NFT.Lender.Types (LenderParams (..))
import Cerra.Lending.Oracle.Orcfax.OnChain (getOrcfaxPrice, isOrcfaxSupported)
import Cerra.Lending.Oracle.Cerra.OnChain (getCerraPrice)
import Cerra.Lending.Utils.Utils
  ( getCS,
    getTN,
    mkNftTokenName,
    getContractInput,
    getContractOutput,
    getContinuingContractOutput,
    assetAmount,
    assetAmountTwoCurrencies,
    lovelaceAmount,
    lovelaceAmountTwoCurrencies,
    assetLength,
    assetLengthTwoCurrencies,
    factoryNFT,
    factoryNFTTwoCurrencies,
    validateFee,
    getLowerBound,
    isNFTExists,
    factoryNFTUnknownState,
    fromJustCustom,
    isUnity,
    getUpperBound
  )
import Cerra.Lending.Utils.Settings
  ( cerraAssetClass,
    oracleFactoryAssetClass,
    treasuryAddress,
    oracleFactorySymbolOrcfax
  )
import Ledger.Value (assetClass, flattenValue, AssetClass, unAssetClass, assetClassValueOf)
import Ledger.Ada (getLovelace, fromValue)
import Cerra.Lending.Utils.OnChainUtils
  ( mustFindScriptDatum,
    scriptDatumExists
  )
import qualified PlutusTx
import PlutusTx.Builtins (multiplyInteger, divideInteger)
import PlutusTx.Prelude
  ( BuiltinData,
    BuiltinString,
    length,
    isNothing,
    fst,
    snd,
    Bool(..),
    ($),
    (&&),
    (==),
    (/=),
    (<),
    (!!),
    Bool,
    Integer,
    (+)
  )

import Cerra.Lending.Utils.Debug (debugError)

{-# INLINEABLE mkLenderScript #-}
mkLenderScript :: Script
mkLenderScript = unMintingPolicyScript originalLenderPolicy

{-# INLINABLE originalLenderPolicyPlutonomy #-}
originalLenderPolicyPlutonomy :: Plutonomy.MintingPolicy
originalLenderPolicyPlutonomy = Plutonomy.mkMintingPolicyScript ($$(PlutusTx.compile [|| \param' -> mkUntypedMintingPolicy $ mkLenderValidator param' ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
  where
    params =
      LenderParams
        { lTreasuryAddress = treasuryAddress,
          lCerraAssetClass = cerraAssetClass,
          lOracleFactoryAssetClass = oracleFactoryAssetClass,
          lOracleFactorySymbolOrcfax = oracleFactorySymbolOrcfax
        }

{-# INLINEABLE originalLenderPolicy #-}
originalLenderPolicy :: MintingPolicy
originalLenderPolicy =
  Plutonomy.optimizeUPLC $
    Plutonomy.mintingPolicyToPlutus originalLenderPolicyPlutonomy

{-# INLINEABLE mkLenderSymbol #-}
mkLenderSymbol :: CurrencySymbol
mkLenderSymbol = scriptCurrencySymbol originalLenderPolicy

{-# INLINEABLE mkLenderValidator #-}
mkLenderValidator :: LenderParams -> BuiltinData -> ScriptContext -> Bool
mkLenderValidator LenderParams { lTreasuryAddress, lCerraAssetClass, lOracleFactoryAssetClass, lOracleFactorySymbolOrcfax } rawRedeemer context =
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

        ownAssetClass = case [c | c <- (flattenValue mintValue), ownSymbol == getCS c] of
          [o] -> assetClass (getCS o) (getTN o)
          _ -> debugError "E3" True

        lenderTokenName = mkNftTokenName ref

        lenderNFT :: AssetClass
        lenderNFT = assetClass ownSymbol lenderTokenName

    in if isUnity mintValue lenderNFT && spendsOutput info refHash refIdx then
            if contractInputExists then validateMintAccept lenderTokenName info
            else validateMintInitial lenderTokenName lTreasuryAddress lCerraAssetClass info
       else if assetClassValueOf mintValue ownAssetClass == -1 then validateBurn (snd $ unAssetClass $ ownAssetClass) lOracleFactoryAssetClass lOracleFactorySymbolOrcfax info
       else False

{-# INLINEABLE validateMintAccept #-}
validateMintAccept :: TokenName -> TxInfo -> Bool
validateMintAccept lenderTokenName info =
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

        !loanInOutput = assetAmountTwoCurrencies loanAssetOut (assetClassValueOf outVal loanAssetOut)
        !collateralInOutput = assetAmountTwoCurrencies collateralAssetOut (assetClassValueOf outVal collateralAssetOut)
        !lovelaceInOutput = lovelaceAmountTwoCurrencies loanAssetOut loanAmountOut collateralAssetOut collateralAmountOut (getLovelace (fromValue outVal))
        !outputLength = assetLengthTwoCurrencies loanAssetOut collateralAssetOut (length outValFlatten)
        !nftInOutput = factoryNFTTwoCurrencies outValFlatten loanAssetOut collateralAssetOut

     in if nftInInput == nftInOutput
         && (length mintValFlatten) == 1
         && loanAssetIn /= collateralAssetIn
         && isNothing lenderNFTIn
         && (fromJustCustom lenderNFTOut) == lenderTokenName
         && fromJustCustom borrowerNFTIn == fromJustCustom borrowerNFTOut
         && oracleAddressLoanIn == oracleAddressLoanOut
         && oracleAddressCollateralIn == oracleAddressCollateralOut
         && loanAssetIn == loanAssetOut
         && loanAmountIn == loanAmountOut
         && collateralAssetIn == collateralAssetOut
         && collateralAmountIn == collateralAmountOut
         && isNothing loanStartTimeIn
         && getUpperBound (txInfoValidRange info) == fromJustCustom loanStartTimeOut
         && loanLengthIn == loanLengthOut
         && interestPerSecondIn == interestPerSecondOut
         && inputLength == 3
         && collateralInInput == collateralAmountIn
         && lovelaceInInput == 5_000_000
         && outputLength == 4
         && loanInOutput == loanAmountOut
         && collateralInOutput == collateralAmountOut
         && lovelaceInOutput == 10_000_000
         then True
         else False

{-# INLINEABLE validateMintInitial #-}
validateMintInitial :: TokenName -> Address -> AssetClass -> TxInfo -> Bool
validateMintInitial lenderTokenName lTreasuryAddress lCerraAssetClass info =
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
        loanAmount        = scLoanAmount positionDatum
        collateralAsset   = scCollateralAsset positionDatum
        loanStartTime     = scLoanStartTime positionDatum

        !loanInOutput = assetAmount loanAsset (assetClassValueOf outVal loanAsset)
        !lovelaceInOutput = lovelaceAmount loanAsset outVal
        !outputLength = assetLength loanAsset (length outValFlatten)
        !nftInOutput = factoryNFT outValFlatten loanAsset

        !cerraInInput = assetClassValueOf valSpent lCerraAssetClass

     in if (fromJustCustom lenderNFT) == lenderTokenName
           && loanAsset /= collateralAsset
           && isNothing borrowerNFT
           && isNothing loanStartTime
           && (length mintValFlatten) == 2
           && outputLength == 3
           && loanInOutput == loanAmount
           && assetClassValueOf mintValue nftInOutput == 1
           && lovelaceInOutput == 5_000_000
           &&
           (
             if validateFee lTreasuryAddress cerraInInput info then True
             else False
           )
         then True
         else False

{-# INLINEABLE validateBurn #-}
validateBurn :: TokenName -> AssetClass -> CurrencySymbol -> TxInfo -> Bool
validateBurn lenderTokenName lOracleFactoryAssetClass lOracleFactorySymbolOrcfax info =
    let mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue

        contractInput = getContractInput info
        !inVal = txOutValue contractInput
        inValFlatten = flattenValue inVal

        !positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
        borrowerNFTIn             = scBorrowerNFT positionDatumIn
        lenderNFTIn               = scLenderNFT positionDatumIn
        loanAssetIn               = scLoanAsset positionDatumIn
        loanAmountIn              = scLoanAmount positionDatumIn
        collateralAssetIn         = scCollateralAsset positionDatumIn
        collateralAmountIn        = scCollateralAmount positionDatumIn
        loanStartTimeIn           = scLoanStartTime positionDatumIn
        loanLengthIn              = scLoanLength positionDatumIn

        !nftInInput = factoryNFTUnknownState inValFlatten collateralAssetIn loanAssetIn

        nowTime :: POSIXTime
        nowTime = getLowerBound (txInfoValidRange info)

        loanEndsAt :: POSIXTime
        loanEndsAt = POSIXTime ((getPOSIXTime (fromJustCustom loanStartTimeIn)) + (multiplyInteger loanLengthIn 1000))

        txReferenceInputs :: [TxInInfo]
        txReferenceInputs = txInfoReferenceInputs info

        oracleInputExists :: Bool
        !oracleInputExists = case [o | o <- txReferenceInputs, scriptDatumExists (txInInfoResolved o)] of
          [_, _] -> True
          _ -> False

        commonConditions = (fromJustCustom lenderNFTIn) == lenderTokenName
          && (length mintValFlatten) == 2
          && assetClassValueOf mintValue nftInInput == -1

     in if isNothing borrowerNFTIn
           then -- as there is no borrower NFT, loan is either not accepted, or already repaid.
                -- we do not need to differentiate these scenarios, as already repaid loan does not introduce any additional validation
                -- successful repayment validation is already enforced on other scenarios/redeemers, we just collect the result in either case at this point
                -- we do not need to check time in this case, as not started loan does not have a time, and not ended loan must have borrower NFT present in datum
             if commonConditions then True else False
           else -- as there is a borrower NFT present, this means loan is currently active
             if loanEndsAt < nowTime
               then -- no matter the utxo state, we have the right to collect the whole utxo for ourselves, as loan period ended
                    -- this can be seen as liquidation based on time.
                 if commonConditions then True else False
               else -- at this point, the only way to end the loan is to liquidate borrower with oracle
                 if oracleInputExists
                      then
                        if isLoanCanBeLiquidated info txReferenceInputs lOracleFactoryAssetClass lOracleFactorySymbolOrcfax loanAssetIn loanAmountIn collateralAssetIn collateralAmountIn
                        && commonConditions then True else False
                      else False

{-# INLINEABLE getOracleData #-}
getOracleData :: TxInfo -> TxOut -> Value -> AssetClass -> CurrencySymbol -> (Integer, (AssetClass, BuiltinString))
getOracleData info input value cerraAsset orcfaxSymbol =
  if assetClassValueOf value cerraAsset == 1
  then getCerraPrice info input
  else if isNFTExists (flattenValue value) orcfaxSymbol
  then getOrcfaxPrice info input
  else debugError "E4" True

{-# INLINEABLE determinePriceBySide #-}
determinePriceBySide :: AssetClass -> (Integer, (AssetClass, BuiltinString)) -> (Integer, (AssetClass, BuiltinString)) -> Integer
determinePriceBySide asset oracleDataOne oracleDataTwo =
  if asset == (fst (snd oracleDataOne)) && isOrcfaxSupported oracleDataOne asset
  then fst oracleDataOne
  else if asset == (fst (snd oracleDataTwo)) && isOrcfaxSupported oracleDataTwo asset
  then fst oracleDataTwo
  else debugError "E5" True

{-# INLINEABLE isLoanCanBeLiquidated #-}
isLoanCanBeLiquidated :: TxInfo -> [TxInInfo] -> AssetClass -> CurrencySymbol -> AssetClass -> Integer -> AssetClass -> Integer -> Bool
isLoanCanBeLiquidated info txReferenceInputs lOracleFactoryAssetClass lOracleFactorySymbolOrcfax loanAsset loanAmount collateralAsset collateralAmount =
    let oracleInputOne = txInInfoResolved (txReferenceInputs !! 0)
        oracleInputTwo = txInInfoResolved (txReferenceInputs !! 1)

        oracleValueOne = txOutValue oracleInputOne
        oracleValueTwo = txOutValue oracleInputTwo

        oracleDataOne = getOracleData info oracleInputOne oracleValueOne lOracleFactoryAssetClass lOracleFactorySymbolOrcfax
        oracleDataTwo = getOracleData info oracleInputTwo oracleValueTwo lOracleFactoryAssetClass lOracleFactorySymbolOrcfax

        loanAssetPrice = determinePriceBySide loanAsset oracleDataOne oracleDataTwo
        collateralAssetPrice = determinePriceBySide collateralAsset oracleDataOne oracleDataTwo

        loanAmountInUsd = multiplyInteger loanAmount loanAssetPrice
        collateralAmountInUsd = multiplyInteger (multiplyInteger collateralAmount collateralAssetPrice) 1000

        ratio = divideInteger collateralAmountInUsd loanAmountInUsd

     in if ratio < 1_050
        then True else False
