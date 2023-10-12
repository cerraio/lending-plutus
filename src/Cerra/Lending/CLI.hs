{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Api hiding (Script)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Cerra.Lending.NFT.Borrower.OnChain
import Cerra.Lending.NFT.Lender.OnChain
import Cerra.Lending.Contract.Lending.OnChain
import Cerra.Lending.NFT.Factory.OnChain
import Ledger (Script)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "compile" -> do
      writePlutusScript' "Lending" "plutus/lending.plutus" mkLendingScript
      writePlutusScript' "Factory" "plutus/factory.plutus" mkNftScript
      writePlutusScript' "Borrower" "plutus/borrower.plutus" mkBorrowerScript
      writePlutusScript' "Lender" "plutus/lender.plutus" mkLenderScript
    _ -> error "Command not supported"

writePlutusScript' :: String -> FilePath -> Script -> IO ()
writePlutusScript' title filename scrpt =
  do
    print title
    let scriptSBS = SBS.toShort . LBS.toStrict . serialise $ scrpt
    let scriptSerial = PlutusScriptSerialised scriptSBS :: PlutusScript PlutusScriptV2
    result <- writeFileTextEnvelope filename Nothing scriptSerial
    case result of
      Left err -> print $ displayError err
      Right () -> return ()
