{-# LANGUAGE ImportQualifiedPost #-}

module Audit where

import Audit.Attacks qualified as Attacks
import Audit.Traces qualified as Traces
import Test.Tasty qualified as Tasty

allTests :: Tasty.TestTree
allTests =
  Tasty.testGroup
    "Audit test suite"
    [ Traces.allTests,
      Attacks.allTests
    ]
