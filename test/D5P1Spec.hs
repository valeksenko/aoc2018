module D5P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D5P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D5P1.polymersize: gives correct answer to the original problem" $ do
            10 @=? D5P1.polymersize "dabAcCaCBAcCcaDA"
    ]

