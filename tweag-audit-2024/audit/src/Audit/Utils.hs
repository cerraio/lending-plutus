{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Audit.Utils where

import Cerra.Lending.Contract.Lending.Types qualified as Cerra
import Cerra.Lending.NFT.Borrower.OnChain qualified as Cerra
import Cerra.Lending.NFT.Factory.OnChain qualified as Cerra
import Cerra.Lending.NFT.Lender.OnChain qualified as Cerra
import Cerra.Lending.Oracle.Cerra.Types qualified as Cerra
import Cerra.Lending.Oracle.Orcfax.Types qualified as Cerra
import Cerra.Lending.Utils.Settings qualified as Cerra
import Cooked qualified as C
import Data.Default (Default (..))
import Data.Map qualified as Map
import Plutus.Script.Utils.V2.Scripts qualified as Pl
import Plutus.Script.Utils.Value qualified as Pl
import Plutus.V2.Ledger.Api qualified as Pl
import PlutusTx.Eq qualified
import Prettyprinter ((<+>))
import Prettyprinter qualified

-------------------------------------------------------
-- Aliases for minting policies and currency symbols --
-- Helpers for creating associated nfts              --
-------------------------------------------------------

factoryPolicy, borrowerPolicy, lenderPolicy :: Pl.MintingPolicy
factoryPolicy = Cerra.originalNftPolicy
borrowerPolicy = Cerra.originalBorrowerPolicy
lenderPolicy = Cerra.originalLenderPolicy

factoryCurrencySymbol, borrowerCurrencySymbol, lenderCurrencySymbol :: Pl.CurrencySymbol
factoryCurrencySymbol = Pl.scriptCurrencySymbol factoryPolicy
borrowerCurrencySymbol = Pl.scriptCurrencySymbol borrowerPolicy
lenderCurrencySymbol = Pl.scriptCurrencySymbol lenderPolicy

mkFactoryToken, mkBorrowerToken, mkLenderToken :: Pl.TokenName -> Pl.Value
mkFactoryToken tokenName = Pl.assetClassValue (Pl.AssetClass (factoryCurrencySymbol, tokenName)) 1
mkBorrowerToken tokenName = Pl.assetClassValue (Pl.AssetClass (borrowerCurrencySymbol, tokenName)) 1
mkLenderToken tokenName = Pl.assetClassValue (Pl.AssetClass (lenderCurrencySymbol, tokenName)) 1

--------------------------------------------------------------
-- Wallets with their associated PubKeyHashes and Addresses --
--------------------------------------------------------------

peers :: [C.Wallet]
alice, bob, carrie, cerraOracle, orcfaxOracle, cerraAdmin :: C.Wallet
peers@[alice, bob, carrie, cerraOracle, orcfaxOracle, cerraAdmin] = C.wallet <$> [1, 2, 3, 4, 5, 6]

alicePkh, bobPkh, carriePkh, cerraOraclePkh, orcfaxOraclePkh, cerraAdminPkh :: Pl.PubKeyHash
[alicePkh, bobPkh, carriePkh, cerraOraclePkh, orcfaxOraclePkh, cerraAdminPkh] = C.walletPKHash <$> peers

aliceAddr, bobAddr, carrieAddr, cerraOracleAddr, orcfaxOracleAddr, cerraAdminAddr :: Pl.Address
[aliceAddr, bobAddr, carrieAddr, cerraOracleAddr, orcfaxOracleAddr, cerraAdminAddr] = C.walletAddress <$> peers

------------------------------------------------------------
-- Useful asset classes and associated smart constructors --
------------------------------------------------------------

bananaAssetClass :: Pl.AssetClass
bananaAssetClass = C.permanentAssetClass "banana"

banana :: Integer -> Pl.Value
banana = Pl.assetClassValue bananaAssetClass

collateralAssetClass :: Pl.AssetClass
collateralAssetClass = C.permanentAssetClass "collateral"

collateral :: Integer -> Pl.Value
collateral = Pl.assetClassValue collateralAssetClass

adaAssetClass :: Pl.AssetClass
adaAssetClass = Pl.AssetClass (Pl.adaSymbol, Pl.adaToken)

lovelace :: Integer -> Pl.Value
lovelace = Pl.assetClassValue adaAssetClass

ada :: Integer -> Pl.Value
ada = lovelace . (* 1_000_000)

minAda :: Pl.Value
minAda = ada 2

cerraOracleFactoryToken :: Integer -> Pl.Value
cerraOracleFactoryToken = Pl.assetClassValue Cerra.oracleFactoryAssetClass

orcfaxOracleFactoryToken :: Integer -> Pl.Value
orcfaxOracleFactoryToken = Pl.assetClassValue (Pl.AssetClass (Cerra.oracleFactorySymbolOrcfax, "orcfaxToken"))

----------------------------------
-- Default initial distribution --
----------------------------------

initDist :: C.InitialDistribution
initDist =
  C.InitialDistribution $
    Map.fromList $
      (cerraOracle, [cerraOracleFactoryToken 10 <> ada 2])
        : (orcfaxOracle, [orcfaxOracleFactoryToken 10 <> ada 2])
        : (cerraAdmin, [ada 100])
        : [(p, [banana 20 <> collateral 10 <> ada 2, ada 100, ada 100, ada 100]) | p <- [alice, bob, carrie]]

-----------------------------------------------
-- Additional instances of the Lending Datum --
-----------------------------------------------

deriving instance Eq Cerra.LendingDatum

instance PlutusTx.Eq.Eq Cerra.LendingDatum where
  (==) = (==)

instance C.PrettyCooked Cerra.LendingDatum where
  prettyCookedOpt pcOpts Cerra.LendingDatum {..} =
    C.prettyItemize
      "LendingDatum"
      "-"
      [ case scBorrowerNFT of
          Nothing -> "No borrower"
          Just tokenName -> "Borrower NFT token name:" <+> Prettyprinter.viaShow tokenName,
        case scLenderNFT of
          Nothing -> "No lender"
          Just tokenName -> "Lender NFT token name:" <+> Prettyprinter.viaShow tokenName,
        "Oracle address loan:" <+> C.prettyCookedOpt pcOpts scOracleAddressLoan,
        "Oracle address collateral:" <+> C.prettyCookedOpt pcOpts scOracleAddressCollateral,
        "Loan:" <+> C.prettyCookedOpt pcOpts scLoanAmount <+> C.prettyCookedOpt pcOpts scLoanAsset,
        "Collateral:" <+> C.prettyCookedOpt pcOpts scCollateralAmount <+> C.prettyCookedOpt pcOpts scCollateralAsset,
        "Start time:" <+> maybe "None" (C.prettyCookedOpt pcOpts) scLoanStartTime,
        "Loan length:" <+> C.prettyCookedOpt pcOpts scLoanLength,
        "Interest per second:" <+> C.prettyCookedOpt pcOpts scInterestPerSecond
      ]

--------------------------------------------------
-- Additional instances of the Lending redeemer --
--------------------------------------------------

deriving instance Eq Cerra.LendingRedeemer

instance PlutusTx.Eq.Eq Cerra.LendingRedeemer where
  (==) = (==)

instance C.PrettyCooked Cerra.LendingRedeemer where
  prettyCookedOpt _ Cerra.ApplyRedeem {..} =
    C.prettyItemize
      "LendingRedeemer"
      "-"
      [ case scEndpoint of
          0 -> "Cancel"
          1 -> "Accept"
          2 -> "Take"
          3 -> "Repay"
          4 -> "Collect"
          5 -> "Liquidate"
          _ -> "Invalid redeemer"
      ]

--------------------------------------------------
-- Additional instances of the CerraOracleDatum --
--------------------------------------------------

deriving instance Eq Cerra.CerraOracleDatum

instance PlutusTx.Eq.Eq Cerra.CerraOracleDatum where
  (==) = (==)

instance C.PrettyCooked Cerra.CerraOracleDatum where
  prettyCookedOpt _ = Prettyprinter.viaShow

---------------------------------------------------
-- Additional instances of the OrcfaxOracleDatum --
---------------------------------------------------

deriving instance Eq Cerra.OrcfaxOracleDatum

instance PlutusTx.Eq.Eq Cerra.OrcfaxOracleDatum where
  (==) = (==)

instance C.PrettyCooked Cerra.OrcfaxOracleDatum where
  prettyCookedOpt _ = Prettyprinter.viaShow

------------------------------
-- Creating a default datum --
------------------------------

instance Default Cerra.LendingDatum where
  def =
    Cerra.LendingDatum
      { Cerra.scBorrowerNFT = Nothing,
        Cerra.scLenderNFT = Nothing,
        Cerra.scOracleAddressLoan = cerraOracleAddr,
        Cerra.scOracleAddressCollateral = cerraOracleAddr,
        Cerra.scLoanAsset = bananaAssetClass,
        Cerra.scLoanAmount = 5,
        Cerra.scCollateralAsset = collateralAssetClass,
        Cerra.scCollateralAmount = 1,
        Cerra.scLoanStartTime = Nothing,
        Cerra.scLoanLength = 3_600, -- an hour
        Cerra.scInterestPerSecond = 1_000_000_000 -- 10^9 / 10^12 = 0.001
      }

-------------------------
-- Helpers from datums --
-------------------------

datumToLoanValue :: Cerra.LendingDatum -> Pl.Value
datumToLoanValue Cerra.LendingDatum {..} = Pl.assetClassValue scLoanAsset scLoanAmount

datumToCollateralValue :: Cerra.LendingDatum -> Pl.Value
datumToCollateralValue Cerra.LendingDatum {..} = Pl.assetClassValue scCollateralAsset scCollateralAmount

--------------------------------
-- Helpers for time and slots --
--------------------------------

getMsPerSlot :: C.MonadBlockChain m => m Integer
getMsPerSlot = do
  (Pl.POSIXTime timestamp1, _) <- C.currentTime
  _ <- C.waitNSlots 1
  (Pl.POSIXTime timestamp2, _) <- C.currentTime
  return (timestamp2 - timestamp1)

---------------------------------
-- Printing an running a trace --
---------------------------------

printAndRun trace = C.printCooked $ fst <$> C.interpretAndRunWith (C.runMockChainTFrom initDist) trace

printAndRun' trace = C.printCooked $ snd <$> C.interpretAndRunWith (C.runMockChainTFrom initDist) trace
