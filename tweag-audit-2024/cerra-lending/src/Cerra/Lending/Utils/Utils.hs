{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant if" #-}

module Cerra.Lending.Utils.Utils (module Cerra.Lending.Utils.Utils) where

import Cerra.Lending.Utils.Debug (debugError)
import Cerra.Lending.Utils.OnChainUtils (integerToBS, scriptDatumExists)
import Data.Maybe (fromJust)
import Ledger.Interval (Extended (Finite), Interval, LowerBound (..), UpperBound (..), ivFrom, ivTo)
import Plutus.Script.Utils.Ada (fromValue, getLovelace)
import Plutus.Script.Utils.Value (AssetClass (..), Value, assetClass, assetClassValue, getValue)
import Plutus.V1.Ledger.Address (toValidatorHash)
import Plutus.V2.Ledger.Api
  ( Address,
    CurrencySymbol,
    ScriptContext,
    TokenName (TokenName),
    TxId (getTxId),
    TxInInfo,
    TxInfo (txInfoInputs),
    TxOut,
    TxOutRef (TxOutRef),
    ValidatorHash,
    Value (Value),
    adaSymbol,
    adaToken,
    txInInfoResolved,
  )
import Plutus.V2.Ledger.Contexts
  ( findOwnInput,
    txInfoOutputs,
    txOutAddress,
    txOutValue,
  )
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude
  ( Bool (..),
    Integer,
    Maybe (..),
    fst,
    sha2_256,
    ($),
    (&&),
    (+),
    (-),
    (/=),
    (<>),
    (==),
    (||),
  )

{-# INLINEABLE lendingNFTOf #-}
lendingNFTOf :: Value -> CurrencySymbol -> AssetClass
lendingNFTOf (Value v) nftSymbol = case Map.lookup nftSymbol v of
  Nothing -> debugError "E13" True
  Just i -> case [o | o@(_, am) <- Map.toList i, am == 1] of
    [(tn, _)] -> assetClass nftSymbol tn
    _ -> debugError "E14" True

{-# INLINEABLE getCS #-}
getCS :: (a, b, c) -> a
getCS (a, _, _) = a

{-# INLINEABLE getTN #-}
getTN :: (a, b, c) -> b
getTN (_, b, _) = b

{-# INLINEABLE getAmount #-}
getAmount :: (a, b, c) -> c
getAmount (_, _, c) = c

{-# INLINEABLE mkNftTokenName #-}
mkNftTokenName :: TxOutRef -> TokenName
mkNftTokenName (TxOutRef refHash refIdx) = tName
  where
    tName :: TokenName
    tName = TokenName $ sha2_256 $ getTxId refHash <> integerToBS refIdx

{-# INLINEABLE getContractInput #-}
getContractInput :: TxInfo -> TxOut
getContractInput info = txInInfoResolved contractInput
  where
    !txInputs = txInfoInputs info

    contractInput :: TxInInfo
    !contractInput = case [i | i <- txInputs, scriptDatumExists (txInInfoResolved i)] of
      [i] -> i
      _ -> debugError "E15" True

{-# INLINEABLE getValidateContractOutput #-}
getValidateContractOutput :: TxInfo -> ValidatorHash -> TxOut
getValidateContractOutput info vh =
  if (fromJustCustom $ toValidatorHash $ txOutAddress scOutput) == vh
    then scOutput
    else debugError "E16" True
  where
    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    scOutput :: TxOut
    !scOutput = case [o | o <- txOutputs, scriptDatumExists o] of
      [o] -> o
      _ -> debugError "E17" True

{-# INLINEABLE getContractOutput #-}
getContractOutput :: TxInfo -> TxOut
getContractOutput info = contractOutput
  where
    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    contractOutput :: TxOut
    !contractOutput = case [o | o <- txOutputs, scriptDatumExists o] of
      [o] -> o
      _ -> debugError "E18" True

{-# INLINEABLE assetAmount #-}
assetAmount :: AssetClass -> Integer -> Integer
assetAmount asset amount = case asset == adaCoin of
  True -> amount - 2_000_000
  False -> amount

{-# INLINEABLE assetAmountTwoCurrencies #-}
assetAmountTwoCurrencies :: AssetClass -> Integer -> Integer
assetAmountTwoCurrencies asset amount = case asset == adaCoin of
  True -> amount - 4_000_000
  False -> amount

{-# INLINEABLE lovelaceAmount #-}
lovelaceAmount :: AssetClass -> Value -> Integer
lovelaceAmount asset val = case asset == adaCoin of
  True -> 2_000_000
  False -> getLovelace (fromValue val)

{-# INLINEABLE lovelaceAmountTwoCurrencies #-}
lovelaceAmountTwoCurrencies :: AssetClass -> Integer -> AssetClass -> Integer -> Integer -> Integer
lovelaceAmountTwoCurrencies asset1 amount1 asset2 amount2 adaBalance =
  -- scenario where both assets are ADA is impossible
  if asset1 == adaCoin
    then adaBalance - amount1
    else
      if asset2 == adaCoin
        then adaBalance - amount2
        else adaBalance

{-# INLINEABLE assetLength #-}
assetLength :: AssetClass -> Integer -> Integer
assetLength asset count = case asset == adaCoin of
  True -> count + 1
  False -> count

{-# INLINEABLE assetLengthTwoCurrencies #-}
assetLengthTwoCurrencies :: AssetClass -> AssetClass -> Integer -> Integer
assetLengthTwoCurrencies asset1 asset2 count = case (asset1 == adaCoin) || (asset2 == adaCoin) of
  True -> count + 1
  False -> count

{-# INLINEABLE factoryNFT #-}
factoryNFT :: [(CurrencySymbol, TokenName, Integer)] -> AssetClass -> AssetClass
factoryNFT flattenVal asset = case [ c | c <- flattenVal, adaSymbol /= getCS c
                                                            && fst (unAssetClass asset) /= getCS c
                                                            && getAmount c == 1
                                   ] of
  [c] -> assetClass (getCS c) (getTN c)
  _ -> debugError "E19" True

{-# INLINEABLE factoryNFTTwoCurrencies #-}
factoryNFTTwoCurrencies :: [(CurrencySymbol, TokenName, Integer)] -> AssetClass -> AssetClass -> AssetClass
factoryNFTTwoCurrencies flattenVal asset1 asset2 = case [ c | c <- flattenVal, adaSymbol /= getCS c
                                                                                 && fst (unAssetClass asset1) /= getCS c
                                                                                 && fst (unAssetClass asset2) /= getCS c
                                                                                 && getAmount c == 1
                                                        ] of
  [c] -> assetClass (getCS c) (getTN c)
  _ -> debugError "E20" True

{-# INLINEABLE factoryNFTUnknownState #-}
factoryNFTUnknownState :: [(CurrencySymbol, TokenName, Integer)] -> AssetClass -> AssetClass -> AssetClass
factoryNFTUnknownState flattenVal assetOne assetTwo = case [ c | c <- flattenVal, adaSymbol /= getCS c
                                                                                    && fst (unAssetClass assetOne) /= getCS c
                                                                                    && fst (unAssetClass assetTwo) /= getCS c
                                                                                    && getAmount c == 1
                                                           ] of
  [c] -> assetClass (getCS c) (getTN c)
  _ -> debugError "E21" True

{-# INLINEABLE isNFTExists #-}
isNFTExists :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> Bool
isNFTExists flattenVal symbol = case [ c | c <- flattenVal, symbol == getCS c
                                                              && getAmount c == 1
                                     ] of
  [_] -> True
  _ -> False

isNFTBurned :: [(CurrencySymbol, TokenName, Integer)] -> CurrencySymbol -> Bool
isNFTBurned flattenVal symbol = case [ c | c <- flattenVal, symbol == getCS c
                                                              && getAmount c == -1
                                     ] of
  [_] -> True
  _ -> False

{-# INLINEABLE ownContractInput #-}
ownContractInput :: ScriptContext -> TxOut
ownContractInput ctx = case findOwnInput ctx of
  Just txInInfo -> txInInfoResolved txInInfo
  _ -> debugError "E22" True

{-# INLINEABLE ownContractOutput #-}
ownContractOutput :: TxInfo -> Address -> TxOut
ownContractOutput info ownAddress = ownOutput
  where
    txOutputs :: [TxOut]
    !txOutputs = txInfoOutputs info

    ownOutput :: TxOut
    !ownOutput = case [o | o <- txOutputs, ownAddress == txOutAddress o] of
      [o] -> o
      _ -> debugError "E23" True

-- Beware --
-- Why is the second argument ignored??
{-# INLINEABLE validateFee #-}
validateFee :: Address -> Integer -> TxInfo -> Bool
validateFee tAddress _ info =
  let treasuryVal = case [o | o <- txInfoOutputs info, tAddress == txOutAddress o] of
        [o] -> txOutValue o
        _ -> debugError "E24" True

      treasuryLovelaceOut = getLovelace (fromValue treasuryVal)
   in if treasuryLovelaceOut == 2_000_000
        then True
        else False

{-# INLINEABLE getUpperBound #-}
getUpperBound :: Interval a -> a
getUpperBound interval = case ivTo interval of
  UpperBound (Finite value) _isInclusive -> fromJust (Just value)
  _ -> debugError "E25" True

{-# INLINEABLE getLowerBound #-}
getLowerBound :: Interval a -> a
getLowerBound interval = case ivFrom interval of
  LowerBound (Finite value) _isInclusive -> fromJust (Just value)
  _ -> debugError "E26" True

{-# INLINEABLE fromJustCustom #-}
fromJustCustom :: Maybe a -> a
fromJustCustom a = case a of
  Just b -> b
  _ -> debugError "E27" True

{-# INLINEABLE unitValue #-}
unitValue :: AssetClass -> Value
unitValue c = assetClassValue c 1

{-# INLINEABLE isUnity #-}
isUnity :: Value -> AssetClass -> Bool
isUnity v c = Map.lookup curr (getValue v) == Just (Map.fromList [(tok, 1)])
  where
    (curr, tok) = unAssetClass c

{-# INLINEABLE adaCoin #-}
adaCoin :: AssetClass
adaCoin = assetClass adaSymbol adaToken

{-# INLINEABLE symbolOf #-}
symbolOf :: AssetClass -> CurrencySymbol
symbolOf (AssetClass (cs, _)) = cs

{-# INLINEABLE tokenNameOf #-}
tokenNameOf :: AssetClass -> TokenName
tokenNameOf (AssetClass (_, tn)) = tn
