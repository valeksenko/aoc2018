module D5P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D5P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D5P2.shortestpolymer: gives correct answer to the original problem" $ do
            4 @=? D5P2.shortestpolymer "dabAcCaCBAcCcaDA"
    ]

