{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Cerra.Lending.NFT.Factory.OnChain
  ( mkNftSymbol,
    mkNftScript,
    originalNftPolicy,
  )
where

import Cerra.Lending.Contract.Lending.OnChain (lendingValidatorHash)
import Cerra.Lending.NFT.Borrower.OnChain (mkBorrowerSymbol)
import Cerra.Lending.NFT.Factory.Types (FactoryParams (..))
import Cerra.Lending.NFT.Lender.OnChain (mkLenderSymbol)
import Cerra.Lending.Utils.Debug (debugError)
import Cerra.Lending.Utils.OnChainUtils (scriptDatumExists)
import Cerra.Lending.Utils.Utils
  ( getContractInput,
    getValidateContractOutput,
    isNFTBurned,
    isNFTExists,
    isUnity,
    lendingNFTOf,
    mkNftTokenName,
  )
import Ledger
  ( toValidatorHash,
  )
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)
import Plutus.Script.Utils.Value (assetClass, assetClassValueOf, flattenValue)
import Plutus.V2.Ledger.Api
  ( CurrencySymbol,
    MintingPolicy,
    Script,
    TxInfo
      ( txInfoMint
      ),
    TxOutRef (TxOutRef),
    ValidatorHash (..),
    mkMintingPolicyScript,
    scriptContextTxInfo,
    txInfoOutputs,
    txOutAddress,
    txOutValue,
    unMintingPolicyScript,
  )
import Plutus.V2.Ledger.Contexts
  ( ScriptContext,
    ownCurrencySymbol,
    spendsOutput,
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinData,
    Maybe (..),
    length,
    ($),
    (&&),
    (==),
    (||),
  )

{-# INLINEABLE mkNftScript #-}
mkNftScript :: Script
mkNftScript = unMintingPolicyScript originalNftPolicy

{-# INLINEABLE originalNftPolicy #-}
originalNftPolicy :: MintingPolicy
originalNftPolicy = mkMintingPolicyScript ($$(PlutusTx.compile [||\param' -> mkUntypedMintingPolicy $ mkNftValidator param'||]) `PlutusTx.applyCode` PlutusTx.liftCode params)
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
mkNftValidator FactoryParams {fValidatorHash, fBorrowerSymbol, fLenderSymbol} rawRedeemer context =
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
        && ( isNFTExists mintValFlatten fBorrowerSymbol
               || isNFTExists mintValFlatten fLenderSymbol
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
        && ( isNFTBurned mintValFlatten fBorrowerSymbol
               || isNFTBurned mintValFlatten fLenderSymbol
           )
        then True
        else False
