module D3P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D3
import qualified D3P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D3P2.uniq: gives correct answer to the original problem" $ do
            (Just "#3") @=? D3P2.uniq [D3.Claim "#1" 1 3 4 4, D3.Claim "#2" 3 1 4 4, D3.Claim "#3" 5 5 2 2]
    ]
