module D6P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D6P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D6P1.largestarea: gives correct answer to the original problem" $ do
            17 @=? D6P1.largestarea [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
    ]

