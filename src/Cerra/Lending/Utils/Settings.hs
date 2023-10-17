{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Cerra.Lending.Utils.Settings
(module Cerra.Lending.Utils.Settings)

where

import Ledger (AssetClass)
import Data.Maybe (fromJust)
import Plutus.V2.Ledger.Api (Address, TokenName, PubKeyHash, CurrencySymbol)
import Ledger.Value (assetClass, currencySymbol, tokenName)
import Ledger.Address (pubKeyHashAddress, PaymentPubKeyHash (PaymentPubKeyHash))
import Text.Hex (Text, decodeHex)
import PlutusTx.Prelude(Maybe(..), ($))

cerraSymbol :: Text.Hex.Text
cerraSymbol = "46f987f7ed1886ba771b077c3ed5bbf3df158f54e0e3fa88d3d1e46e"

{-# INLINEABLE cerraName #-}
cerraName :: TokenName
cerraName = tokenName $ fromJust $ decodeHex "744345525241"

{-# INLINEABLE cerraCurrencySymbol #-}
cerraCurrencySymbol :: CurrencySymbol
cerraCurrencySymbol = currencySymbol $ fromJust $ decodeHex cerraSymbol

{-# INLINEABLE cerraAssetClass #-}
cerraAssetClass :: AssetClass
cerraAssetClass = assetClass cerraCurrencySymbol cerraName

oracleFactory :: Text.Hex.Text
oracleFactory = "089eb57344dcfa1d2d82749566f27aa5c072194d11a261d6e66f33cc"

{-# INLINEABLE oracleFactoryName #-}
oracleFactoryName :: TokenName
oracleFactoryName = tokenName $ fromJust $ decodeHex "4c4943454e5345"

{-# INLINEABLE oracleFactorySymbol #-}
oracleFactorySymbol :: CurrencySymbol
oracleFactorySymbol = currencySymbol $ fromJust $ decodeHex oracleFactory

{-# INLINEABLE oracleFactoryAssetClass #-}
oracleFactoryAssetClass :: AssetClass
oracleFactoryAssetClass = assetClass oracleFactorySymbol oracleFactoryName

{-# INLINEABLE treasuryPubKeyHash #-}
treasuryPubKeyHash :: PubKeyHash
treasuryPubKeyHash = "ec0222eebabb0975456b44bc23dd9baf16bbec620ab4fd10b5dec5c9"

{-# INLINEABLE treasuryPaymentPubKeyHash #-}
treasuryPaymentPubKeyHash :: PaymentPubKeyHash
treasuryPaymentPubKeyHash = PaymentPubKeyHash treasuryPubKeyHash

{-# INLINEABLE treasuryAddress #-}
treasuryAddress :: Address
treasuryAddress = pubKeyHashAddress treasuryPaymentPubKeyHash Nothing