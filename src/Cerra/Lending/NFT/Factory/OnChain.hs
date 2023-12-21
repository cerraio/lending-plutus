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
{-# OPTIONS_GHC -fno-worker-wrapper #-}

module Cerra.Lending.NFT.Factory.OnChain
  ( mkNftSymbol,
    mkNftScript
  )
where

import Plutus.Script.Utils.V2.Typed.Scripts (mkUntypedMintingPolicy)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)

import qualified Plutonomy

import Plutus.V2.Ledger.Api
  ( TxInfo
      ( txInfoMint
      ),
    Script,
    txOutAddress,
    txOutValue,
    scriptContextTxInfo,
    txInfoOutputs,
    CurrencySymbol,
    MintingPolicy,
    unMintingPolicyScript,
    TxOutRef (TxOutRef),
    ValidatorHash (..)
  )
import Plutus.V2.Ledger.Contexts
  ( ownCurrencySymbol,
    spendsOutput,
    ScriptContext
  )
import Ledger
  ( toValidatorHash
  )
import Cerra.Lending.NFT.Factory.Types (FactoryParams (..))
import Cerra.Lending.NFT.Borrower.OnChain (mkBorrowerSymbol)
import Cerra.Lending.NFT.Lender.OnChain (mkLenderSymbol)
import Cerra.Lending.Contract.Lending.OnChain (lendingValidatorHash)
import Cerra.Lending.Utils.Utils
  ( lendingNFTOf,
    mkNftTokenName,
    getContractInput,
    getValidateContractOutput,
    isNFTExists,
    isNFTBurned,
    isUnity
  )
import Cerra.Lending.Utils.OnChainUtils (scriptDatumExists)
import Ledger.Value (flattenValue, assetClassValueOf, assetClass)
import qualified PlutusTx
import PlutusTx.Prelude
  ( BuiltinData,
    Maybe(..),
    Bool(..),
    length,
    ($),
    (&&),
    (||),
    (==)
  )

import Cerra.Lending.Utils.Debug (debugError)

{-# INLINEABLE mkNftScript #-}
mkNftScript :: Script
mkNftScript = unMintingPolicyScript originalNftPolicy

{-# INLINEABLE originalNftPolicy #-}
originalNftPolicy :: MintingPolicy
originalNftPolicy =
  Plutonomy.optimizeUPLC $
    Plutonomy.mintingPolicyToPlutus originalNftPolicyPlutonomy

{-# INLINABLE originalNftPolicyPlutonomy #-}
originalNftPolicyPlutonomy :: Plutonomy.MintingPolicy
originalNftPolicyPlutonomy = Plutonomy.mkMintingPolicyScript ($$(PlutusTx.compile [|| \param' -> mkUntypedMintingPolicy $ mkNftValidator param' ||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
  where
    params =
      FactoryParams
        { fValidatorHash = lendingValidatorHash,
          fBorrowerSymbol = mkBorrowerSymbol,
          fLenderSymbol = mkLenderSymbol
        }

{-# INLINEABLE mkNftSymbol #-}
mkNftSymbol :: CurrencySymbol
mkNftSymbol = scriptCurrencySymbol originalNftPolicy

{-# INLINEABLE mkNftValidator #-}
mkNftValidator :: FactoryParams -> BuiltinData -> ScriptContext -> Bool
mkNftValidator FactoryParams { fValidatorHash, fBorrowerSymbol, fLenderSymbol } rawRedeemer context =
    let info = scriptContextTxInfo context
        ownSymbol = ownCurrencySymbol context

        scriptOutputExists :: Bool
        !scriptOutputExists = case [o | o <- txInfoOutputs info, scriptDatumExists o] of
          [_] -> True
          _ -> False
    in case scriptOutputExists of
          True -> validateMint rawRedeemer info ownSymbol fValidatorHash fBorrowerSymbol fLenderSymbol
          False -> validateBurn info ownSymbol fValidatorHash fBorrowerSymbol fLenderSymbol

{-# INLINEABLE validateMint #-}
validateMint :: BuiltinData -> TxInfo -> CurrencySymbol -> ValidatorHash -> CurrencySymbol -> CurrencySymbol -> Bool
validateMint rawRedeemer info ownSymbol fValidatorHash fBorrowerSymbol fLenderSymbol =
    let ref@(TxOutRef refHash refIdx) = PlutusTx.unsafeFromBuiltinData @TxOutRef rawRedeemer

        mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue

        scriptOutput = getValidateContractOutput info fValidatorHash

        !outVal = txOutValue scriptOutput

        !nftCoinInOutput = lendingNFTOf outVal ownSymbol

    in if isUnity mintValue (assetClass ownSymbol (mkNftTokenName ref))
          && spendsOutput info refHash refIdx
          && assetClassValueOf mintValue nftCoinInOutput == 1
          && (length mintValFlatten) == 2
          &&
          (
            isNFTExists mintValFlatten fBorrowerSymbol
            ||
            isNFTExists mintValFlatten fLenderSymbol
          )
        then True
        else False

{-# INLINEABLE validateBurn #-}
validateBurn :: TxInfo -> CurrencySymbol -> ValidatorHash -> CurrencySymbol -> CurrencySymbol -> Bool
validateBurn info ownSymbol fValidatorHash fBorrowerSymbol fLenderSymbol =
    let mintValue = txInfoMint info
        mintValFlatten = flattenValue mintValue

        contractInput = getContractInput info

        contractValidatorHash = case toValidatorHash (txOutAddress contractInput) of
            Just vh -> vh
            _ -> debugError "E2" True

        !inVal = txOutValue contractInput

        !nftCoinInInput = lendingNFTOf inVal ownSymbol

    in if fValidatorHash == contractValidatorHash
          && assetClassValueOf inVal nftCoinInInput == 1
          && assetClassValueOf mintValue nftCoinInInput == -1
          && (length mintValFlatten) == 2
          &&
          (
            isNFTBurned mintValFlatten fBorrowerSymbol
            ||
            isNFTBurned mintValFlatten fLenderSymbol
          )
        then True
        else False
