{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Audit.Endpoints where

import Audit.Utils
import Cerra.Lending.Contract.Lending.OnChain qualified as Cerra
import Cerra.Lending.Contract.Lending.Types qualified as Cerra
import Cerra.Lending.Oracle.Cerra.Types qualified as Cerra
import Cerra.Lending.Oracle.Orcfax.Types qualified as Cerra
import Cerra.Lending.Utils.Settings qualified as Cerra
import Cerra.Lending.Utils.Utils qualified as Cerra
import Control.Monad (void)
import Cooked qualified as C
import Data.Default
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger qualified as Pl
import Plutus.Script.Utils.Value qualified as Pl
import PlutusTx.Prelude qualified as Pl

-------------------------------------------------
-- Creating a loan from the lender perspective --
-------------------------------------------------

createLoanRequest ::
  C.MonadBlockChain m =>
  -- Whatever modification should be applied to the LendingDatum
  Cerra.LendingDatum ->
  -- The lender
  C.Wallet ->
  -- Whatever txOutRef should be used for nft creation
  Pl.TxOutRef ->
  m (Pl.TxOutRef, Pl.TxOutRef)
createLoanRequest datum lender txOutRef = do
  let Just tokenName = Cerra.scLenderNFT datum
      factoryToken = mkFactoryToken tokenName
      lenderToken = mkLenderToken tokenName
      loanValue = datumToLoanValue datum
  tx <-
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelIns = Map.singleton txOutRef C.TxSkelNoRedeemerForPK,
          C.txSkelOuts =
            [ C.paysScript Cerra.lendingValidator datum (factoryToken <> loanValue <> minAda),
              C.paysPK (C.walletPKHash lender) lenderToken,
              C.paysPK Cerra.treasuryPubKeyHash minAda
            ],
          C.txSkelMints =
            C.txSkelMintsFromList
              [ (Pl.Versioned factoryPolicy Pl.PlutusV2, C.SomeMintsRedeemer txOutRef, tokenName, 1),
                (Pl.Versioned lenderPolicy Pl.PlutusV2, C.SomeMintsRedeemer txOutRef, tokenName, 1)
              ],
          C.txSkelSigners = [lender],
          C.txSkelOpts = def {C.txOptEnsureMinAda = True}
        }
  let (loanTxOutRef, _) : (lenderTxOutRef, _) : _ = C.utxosFromCardanoTx tx
  return (loanTxOutRef, lenderTxOutRef)

--------------------------------
-- Creating an oracle output  --
--------------------------------

createCerraOracleOutput ::
  (C.MonadBlockChain m) =>
  Cerra.CerraOracleDatum ->
  C.Wallet ->
  m Pl.TxOutRef
createCerraOracleOutput datum signer = do
  tx <-
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelOuts =
            [ C.paysPK cerraOraclePkh (cerraOracleFactoryToken 1) `C.withDatum` datum
            ],
          C.txSkelSigners = [signer],
          C.txSkelOpts = def {C.txOptEnsureMinAda = True}
        }
  let (oracleTxOutRef, _) : _ = C.utxosFromCardanoTx tx
  return oracleTxOutRef

createOrcfaxOracleOutput ::
  (C.MonadBlockChain m) =>
  Cerra.OrcfaxOracleDatum ->
  C.Wallet ->
  m Pl.TxOutRef
createOrcfaxOracleOutput datum signer = do
  tx <-
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelOuts =
            [ C.paysPK orcfaxOraclePkh (orcfaxOracleFactoryToken 1) `C.withDatum` datum
            ],
          C.txSkelSigners = [signer],
          C.txSkelOpts = def {C.txOptEnsureMinAda = True}
        }
  let (oracleTxOutRef, _) : _ = C.utxosFromCardanoTx tx
  return oracleTxOutRef

---------------------------------------------------
-- Creating a loan from the borrower perspective --
---------------------------------------------------

createBorrowRequest ::
  C.MonadBlockChain m =>
  -- Whatever modification should be applied to the LendingDatum
  Cerra.LendingDatum ->
  -- The borrower
  C.Wallet ->
  -- Whatever txOutRef should be used for nft creation
  Pl.TxOutRef ->
  m (Pl.TxOutRef, Pl.TxOutRef)
createBorrowRequest datum borrower txOutRef = do
  let Just tokenName = Cerra.scBorrowerNFT datum
      factoryToken = mkFactoryToken tokenName
      borrowerToken = mkBorrowerToken tokenName
      collateralValue = datumToCollateralValue datum
  tx <-
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelIns = Map.singleton txOutRef C.TxSkelNoRedeemerForPK,
          C.txSkelOuts =
            [ C.paysScript Cerra.lendingValidator datum (factoryToken <> collateralValue <> minAda),
              C.paysPK (C.walletPKHash borrower) borrowerToken,
              C.paysPK Cerra.treasuryPubKeyHash minAda
            ],
          C.txSkelMints =
            C.txSkelMintsFromList
              [ (Pl.Versioned factoryPolicy Pl.PlutusV2, C.SomeMintsRedeemer txOutRef, tokenName, 1),
                (Pl.Versioned borrowerPolicy Pl.PlutusV2, C.SomeMintsRedeemer txOutRef, tokenName, 1)
              ],
          C.txSkelSigners = [borrower],
          C.txSkelOpts = def {C.txOptEnsureMinAda = True}
        }
  let (loanTxOutRef, _) : (borrowerTxOutRef, _) : _ = C.utxosFromCardanoTx tx
  return (loanTxOutRef, borrowerTxOutRef)

-- | Host the heavy lending script in a third party UTxO to avoid oversized
-- transactions later
createReferenceLending ::
  C.MonadBlockChain m =>
  -- Wallet who will host the lending script
  C.Wallet ->
  -- Signer
  C.Wallet ->
  m Pl.TxOutRef
createReferenceLending host signer = do
  tx <-
    C.validateTxSkel $
      C.txSkelTemplate
        { C.txSkelIns = Map.empty,
          C.txSkelOuts = [C.paysPK (C.walletPKHash host) mempty `C.withReferenceScript` Cerra.lendingValidator],
          C.txSkelSigners = [signer],
          C.txSkelOpts = def {C.txOptEnsureMinAda = True}
        }
  let (resultTxOutRef, _) : _ = C.utxosFromCardanoTx tx
  return resultTxOutRef

-- | Cancelling a loan request from lender --
cancelLoanRequest ::
  C.MonadBlockChain m =>
  -- | The lender
  C.Wallet ->
  -- | The loan
  Pl.TxOutRef ->
  -- | The lender nft
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the NFT
  Pl.TxOutRef ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  m ()
cancelLoanRequest
  lender
  loanTxOutRef
  lenderTxOutRef
  nftTxOutRef
  referenceLendingTxOutRef = do
    Just loanValue <- C.valueFromTxOutRef loanTxOutRef
    Just lenderValue <- C.valueFromTxOutRef lenderTxOutRef
    let loanMinusNftValue = loanValue <> Pl.negate (mkFactoryToken tokenName)
        lenderMinusNftValue = lenderValue <> Pl.negate (mkLenderToken tokenName)
        tokenName = Cerra.mkNftTokenName nftTxOutRef
    void $
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 0),
                  (lenderTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelOuts = [C.paysPK (C.walletPKHash lender) (loanMinusNftValue <> lenderMinusNftValue)],
            C.txSkelMints =
              C.txSkelMintsFromList
                [ (Pl.Versioned factoryPolicy Pl.PlutusV2, C.NoMintsRedeemer, tokenName, -1),
                  (Pl.Versioned lenderPolicy Pl.PlutusV2, C.SomeMintsRedeemer nftTxOutRef, tokenName, -1)
                ],
            C.txSkelSigners = [lender]
          }

-- | Cancelling a borrower request from borrower --
cancelBorrowRequest ::
  C.MonadBlockChain m =>
  -- | The borrower
  C.Wallet ->
  -- | The loan
  Pl.TxOutRef ->
  -- | The borrower nft
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the NFT
  Pl.TxOutRef ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  m ()
cancelBorrowRequest
  borrower
  loanTxOutRef
  borrowerTxOutRef
  nftTxOutRef
  referenceLendingTxOutRef = do
    Just loanValue <- C.valueFromTxOutRef loanTxOutRef
    Just borrowerValue <- C.valueFromTxOutRef borrowerTxOutRef
    let loanMinusNftValue = loanValue <> Pl.negate (mkFactoryToken tokenName)
        borrowerMinusNftValue = borrowerValue <> Pl.negate (mkBorrowerToken tokenName)
        tokenName = Cerra.mkNftTokenName nftTxOutRef
    void $
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 0),
                  (borrowerTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelOuts = [C.paysPK (C.walletPKHash borrower) (loanMinusNftValue <> borrowerMinusNftValue)],
            C.txSkelMints =
              C.txSkelMintsFromList
                [ (Pl.Versioned factoryPolicy Pl.PlutusV2, C.NoMintsRedeemer, tokenName, -1),
                  (Pl.Versioned borrowerPolicy Pl.PlutusV2, C.SomeMintsRedeemer nftTxOutRef, tokenName, -1)
                ],
            C.txSkelSigners = [borrower]
          }

-- | Accepting a loan request (will put the current time as loan start time)
acceptLoanRequest ::
  C.MonadBlockChain m =>
  -- | The borrower who accepts the loan request
  C.Wallet ->
  -- | The loan
  Pl.TxOutRef ->
  -- | TxOutRef financing the collateral
  Pl.TxOutRef ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  -- | The new lending script utxo and the output to the borrower (containing the loaned value)
  m (Pl.TxOutRef, Pl.TxOutRef)
acceptLoanRequest
  borrower
  loanTxOutRef
  collateralTxOutRef
  referenceLendingTxOutRef = do
    Just loanUtxoValue <- C.valueFromTxOutRef loanTxOutRef
    Just datum <- C.typedDatumFromTxOutRef @Cerra.LendingDatum loanTxOutRef
    -- We mint the borrower NFT by spending the loan utxo, this is an arbitrary choice
    let tokenName = Cerra.mkNftTokenName loanTxOutRef
        loanValue = datumToLoanValue datum
        collateralValue = datumToCollateralValue datum
    (timestamp, _) <- C.currentTime
    slot <- C.currentSlot
    tx <-
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 1),
                  (collateralTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelOuts =
              [ C.paysScript
                  Cerra.lendingValidator
                  ( datum
                      { Cerra.scBorrowerNFT = Just tokenName,
                        Cerra.scLoanStartTime = Just timestamp
                      }
                  )
                  (loanUtxoValue <> Pl.negate (datumToLoanValue datum) <> collateralValue),
                C.paysPK (C.walletPKHash borrower) (loanValue <> mkBorrowerToken tokenName)
              ],
            C.txSkelMints =
              C.txSkelMintsFromList
                [ (Pl.Versioned borrowerPolicy Pl.PlutusV2, C.SomeMintsRedeemer loanTxOutRef, tokenName, 1)
                ],
            C.txSkelValidityRange = Pl.from slot,
            C.txSkelSigners = [borrower],
            C.txSkelOpts = def {C.txOptEnsureMinAda = True}
          }
    let (loanTxOutRef', _) : (borrowerTxOutRef', _) : _ = C.utxosFromCardanoTx tx
    return (loanTxOutRef', borrowerTxOutRef')

-- | Accepting a borrow request
acceptBorrowRequest ::
  C.MonadBlockChain m =>
  -- | The lender who accepts the loan request
  C.Wallet ->
  -- | The loan
  Pl.TxOutRef ->
  -- | TxOutRef financing the loan
  Pl.TxOutRef ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  -- | The new lending script utxo
  m (Pl.TxOutRef, Pl.TxOutRef)
acceptBorrowRequest
  lender
  loanTxOutRef
  lenderTxOutRef
  referenceLendingTxOutRef = do
    Just loanUtxoValue <- C.valueFromTxOutRef loanTxOutRef
    Just datum <- C.typedDatumFromTxOutRef @Cerra.LendingDatum loanTxOutRef
    -- We mint the lender NFT by spending the loan utxo, this is an arbitrary choice
    let tokenName = Cerra.mkNftTokenName loanTxOutRef
        loanValue = datumToLoanValue datum
    (timestamp, _) <- C.currentTime
    slot <- C.currentSlot
    tx <-
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 1),
                  (lenderTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelOuts =
              [ C.paysScript
                  Cerra.lendingValidator
                  ( datum
                      { Cerra.scLenderNFT = Just tokenName,
                        Cerra.scLoanStartTime = Just timestamp
                      }
                  )
                  (loanUtxoValue <> loanValue <> minAda),
                C.paysPK (C.walletPKHash lender) (mkLenderToken tokenName)
              ],
            C.txSkelMints =
              C.txSkelMintsFromList
                [ (Pl.Versioned lenderPolicy Pl.PlutusV2, C.SomeMintsRedeemer loanTxOutRef, tokenName, 1)
                ],
            C.txSkelValidityRange = Pl.from slot,
            C.txSkelSigners = [lender],
            C.txSkelOpts = def {C.txOptEnsureMinAda = True}
          }
    let (loanTxOutRef', _) : (lenderTxOutRef', _) : _ = C.utxosFromCardanoTx tx
    return (loanTxOutRef', lenderTxOutRef')

-- | The borrower withdraws the assets placed there by the lender
withdrawLoan ::
  C.MonadBlockChain m =>
  -- | The borrower who withdraws the loan assets
  C.Wallet ->
  -- | The loan
  Pl.TxOutRef ->
  -- | TxOutRef containing the borrower NFT
  Pl.TxOutRef ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  -- | The new lending script utxo, and the new borrower nft utxo
  m (Pl.TxOutRef, Pl.TxOutRef)
withdrawLoan
  borrower
  loanTxOutRef
  borrowerNftTxOutRef
  referenceLendingTxOutRef = do
    Just loanUtxoValue <- C.valueFromTxOutRef loanTxOutRef
    Just datum <- C.typedDatumFromTxOutRef @Cerra.LendingDatum loanTxOutRef
    -- We mint the lender NFT by spending the loan utxo, this is an arbitrary choice
    let loanValue = datumToLoanValue datum
        Just borrowerTokenName = Cerra.scBorrowerNFT datum
        borrowerToken = mkBorrowerToken borrowerTokenName
    slot <- C.currentSlot
    tx <-
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 2),
                  (borrowerNftTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelOuts =
              [ C.paysScript Cerra.lendingValidator datum (loanUtxoValue <> Pl.negate (loanValue <> minAda)),
                C.paysPK (C.walletPKHash borrower) (loanValue <> minAda <> borrowerToken)
              ],
            -- Warn: this could be parameterized
            C.txSkelValidityRange = Pl.to (slot + Pl.Slot 10),
            C.txSkelSigners = [borrower],
            C.txSkelOpts = def {C.txOptEnsureMinAda = True}
          }
    let (loanTxOutRef', _) : (borrowerTxOutRef', _) : _ = C.utxosFromCardanoTx tx
    return (loanTxOutRef', borrowerTxOutRef')

-- | The borrower withdraws the assets placed there by the lender. We rely on
-- automatic balancing to bring UTxOs containing loan assets in the transaction
-- input to repay the loan and interest. They are not explicit parameters of
-- this endpoint.
repayLoan ::
  C.MonadBlockChain m =>
  -- | The borrower who repays the loan assets
  C.Wallet ->
  -- | The loan
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the NFT
  Pl.TxOutRef ->
  -- | TxOutRef (only pubkey) containing the borrower NFT
  Pl.TxOutRef ->
  -- | Amount of borrowed asset to pay as interest
  Integer ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  -- | The new lending script utxo, and the utxo where the collateral is paid back
  m (Pl.TxOutRef, Pl.TxOutRef)
repayLoan
  borrower
  loanTxOutRef
  nftTxOutRef
  borrowerNftTxOutRef
  interestAmount
  referenceLendingTxOutRef = do
    Just loanUtxoValue <- C.valueFromTxOutRef loanTxOutRef
    Just datum <- C.typedDatumFromTxOutRef @Cerra.LendingDatum loanTxOutRef
    -- We mint the lender NFT by spending the loan utxo, this is an arbitrary choice
    let loanValue = datumToLoanValue datum
        collateralValue = datumToCollateralValue datum
        borrowedAssetClass = Cerra.scLoanAsset datum
        Just borrowerTokenName = Cerra.scBorrowerNFT datum

    slot <- C.currentSlot

    tx <-
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 3),
                  (borrowerNftTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelOuts =
              [ C.paysScript
                  Cerra.lendingValidator
                  datum {Cerra.scBorrowerNFT = Nothing}
                  ( loanUtxoValue
                      <> loanValue
                      <> Pl.assetClassValue borrowedAssetClass interestAmount
                      <> Pl.negate collateralValue
                  ),
                C.paysPK (C.walletPKHash borrower) collateralValue
              ],
            C.txSkelMints =
              C.txSkelMintsFromList
                [(Pl.Versioned borrowerPolicy Pl.PlutusV2, C.SomeMintsRedeemer nftTxOutRef, borrowerTokenName, -1)],
            C.txSkelSigners = [borrower],
            C.txSkelValidityRange = Pl.to slot,
            C.txSkelOpts = def {C.txOptEnsureMinAda = True}
          }
    let (loanTxOutRef', _) : (borrowerTxOutRef', _) : _ = C.utxosFromCardanoTx tx
    return (loanTxOutRef', borrowerTxOutRef')

collectLoan ::
  C.MonadBlockChain m =>
  -- | The lender who collects the loan
  C.Wallet ->
  -- | The loan
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the Factory NFT
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the Lender NFT
  Pl.TxOutRef ->
  -- | TxOutRef (only pubkey) containing the Lender NFT
  Pl.TxOutRef ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  m ()
collectLoan
  lender
  loanTxOutRef
  factoryNftTxOutRef
  lenderNftTxOutRef
  lenderTxOutRef
  referenceLendingTxOutRef = do
    let factoryTokenName = Cerra.mkNftTokenName factoryNftTxOutRef
        factoryNft = mkFactoryToken factoryTokenName
        lenderTokenName = Cerra.mkNftTokenName lenderNftTxOutRef
    Just fullLoanValue <- C.valueFromTxOutRef loanTxOutRef
    void $
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 4),
                  (lenderTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelOuts =
              [ C.paysPK (C.walletPKHash lender) (fullLoanValue <> Pl.negate factoryNft)
              ],
            C.txSkelSigners = [lender],
            C.txSkelMints =
              C.txSkelMintsFromList
                [ (Pl.Versioned lenderPolicy Pl.PlutusV2, C.SomeMintsRedeemer lenderNftTxOutRef, lenderTokenName, -1),
                  (Pl.Versioned factoryPolicy Pl.PlutusV2, C.SomeMintsRedeemer factoryNftTxOutRef, factoryTokenName, -1)
                ]
          }

liquidateOnExpiration ::
  C.MonadBlockChain m =>
  -- | The lender who collects the collateral
  C.Wallet ->
  -- | The dealine of the transaction
  Pl.Slot ->
  -- | The loan
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the Factory NFT
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the Lender NFT
  Pl.TxOutRef ->
  -- | TxOutRef (only pubkey) containing the Lender NFT
  Pl.TxOutRef ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  m ()
liquidateOnExpiration
  lender
  deadline
  loanTxOutRef
  factoryNftTxOutRef
  lenderNftTxOutRef
  lenderTxOutRef
  referenceLendingTxOutRef = do
    let factoryTokenName = Cerra.mkNftTokenName factoryNftTxOutRef
        factoryNft = mkFactoryToken factoryTokenName
        lenderTokenName = Cerra.mkNftTokenName lenderNftTxOutRef
    Just fullLoanValue <- C.valueFromTxOutRef loanTxOutRef
    void $
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 5),
                  (lenderTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelOuts =
              [ C.paysPK (C.walletPKHash lender) (fullLoanValue <> Pl.negate factoryNft)
              ],
            C.txSkelSigners = [lender],
            C.txSkelValidityRange = Pl.to deadline,
            C.txSkelMints =
              C.txSkelMintsFromList
                [ (Pl.Versioned lenderPolicy Pl.PlutusV2, C.SomeMintsRedeemer lenderNftTxOutRef, lenderTokenName, -1),
                  (Pl.Versioned factoryPolicy Pl.PlutusV2, C.SomeMintsRedeemer factoryNftTxOutRef, factoryTokenName, -1)
                ]
          }

liquidateOnRatioThreshold ::
  C.MonadBlockChain m =>
  -- | The lender who collects the collateral
  C.Wallet ->
  -- | A couple of oracles: either two Cerras, or one Cerra and one Orcfax that
  -- explicits the loss of relative value of the collateral
  (Pl.TxOutRef, Pl.TxOutRef) ->
  -- | The loan
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the Factory NFT
  Pl.TxOutRef ->
  -- | TxOutRef that initialized the Lender NFT
  Pl.TxOutRef ->
  -- | TxOutRef (only pubkey) containing the Lender NFT
  Pl.TxOutRef ->
  -- | TxOutRef that hosts the lending script
  Pl.TxOutRef ->
  m ()
liquidateOnRatioThreshold
  lender
  (cerraOracleTxOutRef, otherOracleTxOutRef)
  loanTxOutRef
  factoryNftTxOutRef
  lenderNftTxOutRef
  lenderTxOutRef
  referenceLendingTxOutRef = do
    let factoryTokenName = Cerra.mkNftTokenName factoryNftTxOutRef
        factoryNft = mkFactoryToken factoryTokenName
        lenderTokenName = Cerra.mkNftTokenName lenderNftTxOutRef
    Just fullLoanValue <- C.valueFromTxOutRef loanTxOutRef
    now <- C.currentSlot
    void $
      C.validateTxSkel $
        C.txSkelTemplate
          { C.txSkelIns =
              Map.fromList
                [ (loanTxOutRef, C.TxSkelRedeemerForReferencedScript referenceLendingTxOutRef $ Cerra.ApplyRedeem 5),
                  (lenderTxOutRef, C.TxSkelNoRedeemerForPK)
                ],
            C.txSkelInsReference = Set.fromList [cerraOracleTxOutRef, otherOracleTxOutRef],
            C.txSkelOuts =
              [ C.paysPK (C.walletPKHash lender) (fullLoanValue <> Pl.negate factoryNft)
              ],
            C.txSkelSigners = [lender],
            C.txSkelValidityRange = Pl.interval now now,
            C.txSkelMints =
              C.txSkelMintsFromList
                [ (Pl.Versioned lenderPolicy Pl.PlutusV2, C.SomeMintsRedeemer lenderNftTxOutRef, lenderTokenName, -1),
                  (Pl.Versioned factoryPolicy Pl.PlutusV2, C.SomeMintsRedeemer factoryNftTxOutRef, factoryTokenName, -1)
                ]
          }
