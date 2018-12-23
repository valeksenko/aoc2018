module D23P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D23P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D23P2.shortestdistance: gives correct answer to the original problem" $ do
            36 @=? shortestdistance 3 [((10,12,12), 2), ((12,14,12), 2), ((16,12,12), 4), ((14,14,14), 6), ((50,50,50), 200), ((10,10,10), 5)]
    ]