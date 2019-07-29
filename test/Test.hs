module Main where

import System.IO.Capture

import Test.DocTest
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Exception
import Control.Monad

import Readme

main = do
    doctest ["--fast", "-isrc", "src/Shh/Internal.hs"]
    defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, properties]

properties :: TestTree
properties = testGroup "Properties"
    [ testProperty "String round-trip" $ \(ASCIIString s) -> ioProperty $ do
        r <- captureOutput $ putStr s
        pure $ s == r
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ testCase "README" Readme.test
    ]
