module D1P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D1P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D1P2.findfrequency: gives correct answer to the original problem" $ do
            10 @=? D1P2.findfrequency [3,3,4,-2,-4]
            0 @=? D1P2.findfrequency [1,-1]
            5 @=? D1P2.findfrequency [-6,3,8,5,-6]
            14 @=? D1P2.findfrequency [7,7,-2,-7,-4]

    ]