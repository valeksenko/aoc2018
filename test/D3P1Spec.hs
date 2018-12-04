module D3P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D3
import qualified D3P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D3P1.overlaps: gives correct answer to the original problem" $ do
            4 @=? D3P1.overlaps [D3.Claim "#1" 1 3 4 4, D3.Claim "#2" 3 1 4 4, D3.Claim "#3" 5 5 2 2]
    ]
