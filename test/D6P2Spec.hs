module D6P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D6P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D6P2.region: gives correct answer to the original problem" $ do
            16 @=? D6P2.region 32 [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
    ]

