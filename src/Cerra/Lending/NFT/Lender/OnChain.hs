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
        scOracleAddress,
        scLoanAsset,
        scLoanAmount,
        scCollateralAsset,
        scCollateralAmount,
        scLoanStartTime,
        scLoanLength,
        scInterestPerSecond
      )
  )
import Cerra.Lending.NFT.Lender.Types
  ( LenderParams (..),
    OracleDatum (..)
  )
import Cerra.Lending.Utils.Utils
  ( cerraAssetClass,
    oracleFactoryAssetClass,
    treasuryAddress,
    getCS,
    getTN,
    mkNftTokenName,
    getContractInput,
    getContractOutput,
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
    getUpperBound,
    factoryNFTUnknownState,
    validateOutputAddress,
    fromJustCustom,
    isUnity
  )
import Ledger.Value (assetClass, flattenValue, AssetClass, unAssetClass, assetClassValueOf)
import Ledger.Ada (getLovelace, fromValue)
import Cerra.Lending.Utils.OnChainUtils
  ( mustFindScriptDatum,
    scriptDatumExists
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( BuiltinData,
    error,
    length,
    isNothing,
    Bool(..),
    ($),
    (&&),
    (==),
    (/=),
    (<),
    (>),
    (!!),
    Bool,
    Integer,
    (+),
    (-),
    snd
  )

import PlutusTx.Builtins (divideInteger, multiplyInteger)

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
          lOracleFactoryAssetClass = oracleFactoryAssetClass
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
mkLenderValidator LenderParams { lTreasuryAddress, lCerraAssetClass, lOracleFactoryAssetClass } rawRedeemer context =
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
          _ -> error ()

        lenderTokenName = mkNftTokenName ref

        lenderNFT :: AssetClass
        lenderNFT = assetClass ownSymbol lenderTokenName

    in if isUnity mintValue lenderNFT && spendsOutput info refHash refIdx then
            if contractInputExists then validateMintAccept lenderTokenName info
            else validateMintInitial lenderTokenName lTreasuryAddress lCerraAssetClass info
       else if assetClassValueOf mintValue ownAssetClass == -1 then validateBurn (snd $ unAssetClass $ ownAssetClass) lOracleFactoryAssetClass info
       else False

{-# INLINEABLE validateMintAccept #-}
validateMintAccept :: TokenName -> TxInfo -> Bool
validateMintAccept lenderTokenName info =
    let mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue

        contractInput = getContractInput info
        !inVal = txOutValue contractInput
        inValFlatten = flattenValue inVal

        scriptOutput = getContractOutput info
        !outVal = txOutValue scriptOutput
        outValFlatten = flattenValue outVal

        !positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
        borrowerNFTIn       = scBorrowerNFT positionDatumIn
        lenderNFTIn         = scLenderNFT positionDatumIn
        oracleAddressIn     = scOracleAddress positionDatumIn
        loanAssetIn         = scLoanAsset positionDatumIn
        loanAmountIn        = scLoanAmount positionDatumIn
        collateralAssetIn   = scCollateralAsset positionDatumIn
        collateralAmountIn  = scCollateralAmount positionDatumIn
        loanStartTimeIn     = scLoanStartTime positionDatumIn
        loanLengthIn        = scLoanLength positionDatumIn
        interestPerSecondIn = scInterestPerSecond positionDatumIn

        !positionDatumOut = mustFindScriptDatum @LendingDatum scriptOutput info
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
         && oracleAddressIn == oracleAddressOut
         && loanAssetIn == loanAssetOut
         && loanAmountIn == loanAmountOut
         && collateralAssetIn == collateralAssetOut
         && collateralAmountIn == collateralAmountOut
         && isNothing loanStartTimeIn
         && getLowerBound (txInfoValidRange info) == fromJustCustom loanStartTimeOut
         && loanLengthIn == loanLengthOut
         && interestPerSecondIn == interestPerSecondOut
         && inputLength == 3
         && collateralInInput == collateralAmountIn
         && lovelaceInInput == 2_000_000
         && outputLength == 4
         && loanInOutput == loanAmountOut
         && collateralInOutput == collateralAmountOut
         && lovelaceInOutput == 4_000_000
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
           && lovelaceInOutput == 2_000_000
           &&
           (
             if validateFee lTreasuryAddress cerraInInput info then True
             else False
           )
         then True
         else False

{-# INLINEABLE validateBurn #-}
validateBurn :: TokenName -> AssetClass -> TxInfo -> Bool
validateBurn lenderTokenName lOracleFactoryAssetClass info =
    let mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue

        contractInput = getContractInput info
        !inVal = txOutValue contractInput
        inValFlatten = flattenValue inVal

        !positionDatumIn = mustFindScriptDatum @LendingDatum contractInput info
        borrowerNFTIn       = scBorrowerNFT positionDatumIn
        lenderNFTIn         = scLenderNFT positionDatumIn
        oracleAddressIn     = scOracleAddress positionDatumIn
        loanAssetIn         = scLoanAsset positionDatumIn
        loanAmountIn        = scLoanAmount positionDatumIn
        collateralAssetIn   = scCollateralAsset positionDatumIn
        collateralAmountIn  = scCollateralAmount positionDatumIn
        loanStartTimeIn     = scLoanStartTime positionDatumIn
        loanLengthIn        = scLoanLength positionDatumIn

        !nftInInput = factoryNFTUnknownState inValFlatten collateralAssetIn loanAssetIn

        nowTime :: POSIXTime
        nowTime = getUpperBound (txInfoValidRange info)

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
                        if isLoanCanBeLiquidated info txReferenceInputs lOracleFactoryAssetClass oracleAddressIn loanAssetIn loanAmountIn collateralAssetIn collateralAmountIn nowTime
                        && commonConditions then True else False
                      else False

-- TODO: we are using our own oracle utilizing reference inputs
-- TODO: we can easily add Orcfax ADA price feed using their reference input specifically for ADA (as other tokens are not yet realized on Orcfax side)
{-# INLINEABLE isLoanCanBeLiquidated #-}
isLoanCanBeLiquidated :: TxInfo -> [TxInInfo] -> AssetClass -> Address -> AssetClass -> Integer -> AssetClass -> Integer -> POSIXTime -> Bool
isLoanCanBeLiquidated info txReferenceInputs lOracleFactoryAssetClass oracleAddress loanAsset loanAmount collateralAsset collateralAmount nowTime =
    let oracleInputOne = validateOutputAddress (txInInfoResolved (txReferenceInputs !! 0)) oracleAddress
        oracleInputTwo = validateOutputAddress (txInInfoResolved (txReferenceInputs !! 1)) oracleAddress

        oracleValueOne = txOutValue oracleInputOne
        oracleValueTwo = txOutValue oracleInputTwo

        !oracleDatumOne = mustFindScriptDatum @OracleDatum oracleInputOne info
        assetOne       = oAsset oracleDatumOne
        updateTimeOne  = oUpdateTime oracleDatumOne
        priceOne       = oPrice oracleDatumOne

        !oracleDatumTwo = mustFindScriptDatum @OracleDatum oracleInputTwo info
        assetTwo       = oAsset oracleDatumTwo
        updateTimeTwo  = oUpdateTime oracleDatumTwo
        priceTwo       = oPrice oracleDatumTwo

        lastDay = nowTime - 8_6400_000

        loanAssetPrice = if loanAsset == assetOne
          then priceOne
          else if loanAsset == assetTwo
          then priceTwo
          else error ()

        collateralAssetPrice = if collateralAsset == assetOne
          then priceOne
          else if collateralAsset == assetTwo
          then priceTwo
          else error ()

        loanAmountInUsd = multiplyInteger loanAmount loanAssetPrice
        collateralAmountInUsd = multiplyInteger (multiplyInteger collateralAmount collateralAssetPrice) 1000

        ratio = divideInteger collateralAmountInUsd loanAmountInUsd

     in if updateTimeOne > lastDay
           && updateTimeTwo > lastDay
           && assetClassValueOf oracleValueOne lOracleFactoryAssetClass == 1
           && assetClassValueOf oracleValueTwo lOracleFactoryAssetClass == 1
           && ratio < 1_050
        then True else False
