module Main where

import Audit qualified
import Test.Tasty qualified as Tasty

main :: IO ()
main = Tasty.defaultMain Audit.allTests
