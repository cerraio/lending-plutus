{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Lending.Utils.Settings (module Cerra.Lending.Utils.Settings) where

import Data.Maybe (fromJust)
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash), pubKeyHashAddress)
import Plutus.Script.Utils.Value (AssetClass, assetClass, currencySymbol, tokenName)
import Plutus.V2.Ledger.Api (Address, CurrencySymbol, PubKeyHash, TokenName)
import PlutusTx.Prelude (Maybe (..), ($))
import Text.Hex (Text, decodeHex)

cerraSymbol :: Text.Hex.Text
cerraSymbol = "4342a3d3c15545a592bf38294dc75c7a1dd3550388303e3a06f4416d"

{-# INLINEABLE cerraName #-}
cerraName :: TokenName
cerraName = tokenName $ fromJust $ decodeHex "4345525241"

{-# INLINEABLE cerraCurrencySymbol #-}
cerraCurrencySymbol :: CurrencySymbol
cerraCurrencySymbol = currencySymbol $ fromJust $ decodeHex cerraSymbol

{-# INLINEABLE cerraAssetClass #-}
cerraAssetClass :: AssetClass
cerraAssetClass = assetClass cerraCurrencySymbol cerraName

oracleFactory :: Text.Hex.Text
oracleFactory = "1900714de0db4461660063f854d62730966e5cf9e5a737506877cdda"

{-# INLINEABLE oracleFactoryName #-}
oracleFactoryName :: TokenName
oracleFactoryName = tokenName $ fromJust $ decodeHex "4c4943454e5345"

{-# INLINEABLE oracleFactorySymbol #-}
oracleFactorySymbol :: CurrencySymbol
oracleFactorySymbol = currencySymbol $ fromJust $ decodeHex oracleFactory

{-# INLINEABLE oracleFactoryAssetClass #-}
oracleFactoryAssetClass :: AssetClass
oracleFactoryAssetClass = assetClass oracleFactorySymbol oracleFactoryName

oracleFactoryOrcfax :: Text.Hex.Text
oracleFactoryOrcfax = "2cccc05192920ff1eb02bcfa7bb2a1fc5352ce58391d7ba3c66a555b"

{-# INLINEABLE oracleFactorySymbolOrcfax #-}
oracleFactorySymbolOrcfax :: CurrencySymbol
oracleFactorySymbolOrcfax = currencySymbol $ fromJust $ decodeHex oracleFactoryOrcfax

{-# INLINEABLE treasuryPubKeyHash #-}
treasuryPubKeyHash :: PubKeyHash
treasuryPubKeyHash = "0b8303e11b20e0acdb40394c0e11e124625ce5f482aaa5c449daa2aa"

{-# INLINEABLE treasuryPaymentPubKeyHash #-}
treasuryPaymentPubKeyHash :: PaymentPubKeyHash
treasuryPaymentPubKeyHash = PaymentPubKeyHash treasuryPubKeyHash

{-# INLINEABLE treasuryAddress #-}
treasuryAddress :: Address
treasuryAddress = pubKeyHashAddress treasuryPaymentPubKeyHash Nothing
