{-# LANGUAGE TypeApplications #-}

module Audit.Traces where

import Audit.Endpoints qualified as EP
import Audit.Utils
import Cerra.Lending.Contract.Lending.Types qualified as Cerra
import Cerra.Lending.Oracle.Cerra.Types qualified as Cerra
import Cerra.Lending.Utils.Utils qualified as Cerra
import Control.Monad (void)
import Cooked qualified as C
import Data.Default
import Data.Map qualified as Map
import Plutus.V2.Ledger.Api qualified as Pl
import Test.Tasty qualified as Tasty
import Test.Tasty.ExpectedFailure qualified as Tasty
import Test.Tasty.HUnit qualified as Tasty

allTests :: Tasty.TestTree
allTests =
  Tasty.testGroup
    "Manual traces"
    [ Tasty.testCase "Creation of a loan request" $
        C.testSucceedsFrom def initDist createLoanRequest,
      Tasty.testCase "Creation of a borrow request" $
        C.testSucceedsFrom def initDist createBorrowRequest,
      Tasty.testCase "Acceptance of a loan request" $
        C.testSucceedsFrom def initDist acceptLoanRequest,
      Tasty.testCase "Acceptance of a borrow request" $
        C.testSucceedsFrom def initDist acceptBorrowRequest,
      Tasty.testCase "Cancellation of a loan request" $
        C.testSucceedsFrom def initDist cancelLoanRequest,
      Tasty.testCase "Cancellation of a borrow request" $
        C.testSucceedsFrom def initDist cancelBorrowRequest,
      Tasty.testCase "Withdrawal of a loan" $
        C.testSucceedsFrom def initDist withdrawLoan,
      Tasty.testCase "Reimbursement of a loan" $
        C.testSucceedsFrom def initDist repayLoan,
      Tasty.testCase "Collection of a loan" $
        C.testSucceedsFrom def initDist collectLoan,
      Tasty.testCase "Liquidation of a loan after the deadline" $
        C.testSucceedsFrom def initDist liquidateOnExpiration,
      Tasty.ignoreTest $
        Tasty.testCase "Liquidation of a loan after the collateral is not worth enough" $
          C.testSucceedsFrom def initDist liquidateOnRatioThreshold
    ]

createLoanRequest :: C.MonadBlockChain m => m (Pl.TxOutRef, Pl.TxOutRef, Pl.TxOutRef, Pl.TxOutRef)
createLoanRequest = do
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr
  let datum = def {Cerra.scLenderNFT = Just $ Cerra.mkNftTokenName aliceTxOutRef}
  (loanTxOutRef, lenderTxOutRef) <- EP.createLoanRequest datum alice aliceTxOutRef
  referenceLendingTxOutRef <- EP.createReferenceLending cerraAdmin cerraAdmin
  return (loanTxOutRef, lenderTxOutRef, aliceTxOutRef, referenceLendingTxOutRef)

cancelLoanRequest :: C.MonadBlockChain m => m ()
cancelLoanRequest = do
  (loanTxOutRef, lenderTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createLoanRequest
  EP.cancelLoanRequest alice loanTxOutRef lenderTxOutRef nftTxOutRef referenceLendingTxOutRef

acceptLoanRequest :: C.MonadBlockChain m => m ()
acceptLoanRequest = do
  (loanTxOutRef, _, _, referenceLendingTxOutRef) <- createLoanRequest
  (bobTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch bobAddr
  void $ EP.acceptLoanRequest bob loanTxOutRef bobTxOutRef referenceLendingTxOutRef

createBorrowRequest :: C.MonadBlockChain m => m (Pl.TxOutRef, Pl.TxOutRef, Pl.TxOutRef, Pl.TxOutRef)
createBorrowRequest = do
  (bobTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch bobAddr
  let datum = def {Cerra.scBorrowerNFT = Just $ Cerra.mkNftTokenName bobTxOutRef}
  referenceLendingTxOutRef <- EP.createReferenceLending cerraAdmin cerraAdmin
  (loanTxOutRef, borrowerTxOutRef) <- EP.createBorrowRequest datum bob bobTxOutRef
  return (loanTxOutRef, borrowerTxOutRef, bobTxOutRef, referenceLendingTxOutRef)

cancelBorrowRequest :: C.MonadBlockChain m => m ()
cancelBorrowRequest = do
  (loanTxOutRef, borrowerTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createBorrowRequest
  EP.cancelBorrowRequest bob loanTxOutRef borrowerTxOutRef nftTxOutRef referenceLendingTxOutRef

acceptBorrowRequest :: C.MonadBlockChain m => m ()
acceptBorrowRequest = do
  (loanTxOutRef, _, _, referenceLendingTxOutRef) <- createBorrowRequest
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr
  void $ EP.acceptBorrowRequest alice loanTxOutRef aliceTxOutRef referenceLendingTxOutRef

withdrawLoan :: C.MonadBlockChain m => m ()
withdrawLoan = do
  (loanTxOutRef, bobTxOutRef, _, referenceLendingTxOutRef) <- createBorrowRequest
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr
  (loanTxOutRef', _) <- EP.acceptBorrowRequest alice loanTxOutRef aliceTxOutRef referenceLendingTxOutRef
  void $ EP.withdrawLoan bob loanTxOutRef' bobTxOutRef referenceLendingTxOutRef

repayLoan :: C.MonadBlockChain m => m ()
repayLoan = do
  (loanTxOutRef, bobTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createBorrowRequest
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr

  (loanTxOutRef', _) <- EP.acceptBorrowRequest alice loanTxOutRef aliceTxOutRef referenceLendingTxOutRef
  (loanTxOutRef'', withdrawnAssetsTxOutRef) <- EP.withdrawLoan bob loanTxOutRef' bobTxOutRef referenceLendingTxOutRef

  -- Let's wait a little to require to pay back interest
  msPerSlot <- getMsPerSlot
  let slotsDelay = (20 * 60 * 1000) `div` msPerSlot -- roughly 20 min
  _ <- C.waitNSlots slotsDelay

  -- The following splits the borrower NFT and withdrawn assets in 3 different
  -- utxos, this is just to simulate that the borrower may use the assets and
  -- in the end things are in different utxos
  Just datum <- C.typedDatumFromTxOutRef @Cerra.LendingDatum loanTxOutRef''
  let Just borrowerTokenName = Cerra.scBorrowerNFT datum
      borrowerToken = mkBorrowerToken borrowerTokenName
  tx <-
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelIns = Map.singleton withdrawnAssetsTxOutRef C.TxSkelNoRedeemerForPK,
          C.txSkelOuts = C.paysPK (C.walletPKHash bob) <$> [borrowerToken, banana 3, banana 2],
          C.txSkelSigners = [bob],
          C.txSkelOpts = def {C.txOptEnsureMinAda = True}
        }
  let (borrowerTokenTxOutRef, _) : _ = C.utxosFromCardanoTx tx

  -- Compute the interest
  (_, nowTime) <- C.currentTime
  let (Just loanStartTime) = Cerra.scLoanStartTime datum
      loanAmount = Cerra.scLoanAmount datum
      interestPerSecond = Cerra.scInterestPerSecond datum
      interest = ((((Pl.getPOSIXTime nowTime - Pl.getPOSIXTime loanStartTime) `div` 1_000) * interestPerSecond) * loanAmount) `div` 1_000_000_000_000

  -- Repay the loan with interest
  _ <-
    EP.repayLoan
      bob
      loanTxOutRef''
      nftTxOutRef
      borrowerTokenTxOutRef
      interest
      referenceLendingTxOutRef

  return ()

collectLoan :: C.MonadBlockChain m => m ()
collectLoan = do
  (loanTxOutRef, bobTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createBorrowRequest
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr

  (loanTxOutRef', aliceTxOutRef') <- EP.acceptBorrowRequest alice loanTxOutRef aliceTxOutRef referenceLendingTxOutRef
  (loanTxOutRef'', withdrawnAssetsTxOutRef) <- EP.withdrawLoan bob loanTxOutRef' bobTxOutRef referenceLendingTxOutRef

  -- Let's wait a little to require to pay back interest
  msPerSlot <- getMsPerSlot
  let slotsDelay = (20 * 60 * 1000) `div` msPerSlot -- roughly 20 min
  _ <- C.waitNSlots slotsDelay

  Just datum <- C.typedDatumFromTxOutRef @Cerra.LendingDatum loanTxOutRef''

  -- Compute the interest
  (_, nowTime) <- C.currentTime
  let (Just loanStartTime) = Cerra.scLoanStartTime datum
      loanAmount = Cerra.scLoanAmount datum
      interestPerSecond = Cerra.scInterestPerSecond datum
      interest = ((((Pl.getPOSIXTime nowTime - Pl.getPOSIXTime loanStartTime) `div` 1_000) * interestPerSecond) * loanAmount) `div` 1_000_000_000_000

  -- Repay the loan with interest
  (loanTxOutRef''', _) <-
    EP.repayLoan
      bob
      loanTxOutRef''
      nftTxOutRef
      withdrawnAssetsTxOutRef
      interest
      referenceLendingTxOutRef
  EP.collectLoan alice loanTxOutRef''' nftTxOutRef loanTxOutRef aliceTxOutRef' referenceLendingTxOutRef

liquidateOnExpiration :: C.MonadBlockChain m => m ()
liquidateOnExpiration = do
  (loanTxOutRef, bobTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createBorrowRequest
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr

  (loanTxOutRef', aliceTxOutRef') <- EP.acceptBorrowRequest alice loanTxOutRef aliceTxOutRef referenceLendingTxOutRef
  (loanTxOutRef'', _) <- EP.withdrawLoan bob loanTxOutRef' bobTxOutRef referenceLendingTxOutRef

  -- Let's wait a little to exceed the loan deadline
  msPerSlot <- getMsPerSlot
  now <- C.waitNSlots $ (100 * 60 * 1000) `div` msPerSlot

  -- Repay the loan with interest
  EP.liquidateOnExpiration alice now loanTxOutRef'' nftTxOutRef loanTxOutRef aliceTxOutRef' referenceLendingTxOutRef

-- FIXME DOES NOT WORK YET
liquidateOnRatioThreshold :: C.MonadBlockChain m => m ()
liquidateOnRatioThreshold = do
  (loanTxOutRef, bobTxOutRef, nftTxOutRef, referenceLendingTxOutRef) <- createBorrowRequest
  (aliceTxOutRef, _) : _ <- C.runUtxoSearch $ C.utxosAtSearch aliceAddr

  (loanTxOutRef', aliceTxOutRef') <- EP.acceptBorrowRequest alice loanTxOutRef aliceTxOutRef referenceLendingTxOutRef
  (loanTxOutRef'', _) <- EP.withdrawLoan bob loanTxOutRef' bobTxOutRef referenceLendingTxOutRef

  -- Let's wait a little to exceed the loan deadline
  (now, _) <- C.currentTime

  oracleBananaTxOutRef <- EP.createCerraOracleOutput (Cerra.CerraOracleDatum bananaAssetClass now 10_000_000_000_000) cerraOracle
  oracleCollateralTxOutRef <- EP.createCerraOracleOutput (Cerra.CerraOracleDatum collateralAssetClass now 10_000_000_000_000) cerraOracle

  -- Repay the loan with interest
  EP.liquidateOnRatioThreshold alice (oracleBananaTxOutRef, oracleCollateralTxOutRef) loanTxOutRef'' nftTxOutRef loanTxOutRef aliceTxOutRef' referenceLendingTxOutRef
