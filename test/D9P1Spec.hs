module D9P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D9P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D9P1.winscore: gives correct answer to the original problem" $ do
            32 @=? D9P1.winscore 25 9
            8317 @=? D9P1.winscore 1618 10
            146373 @=? D9P1.winscore 7999 13
            2764 @=? D9P1.winscore 1104 17
            54718 @=? D9P1.winscore 6111 21
            37305 @=? D9P1.winscore 5807 30
    ]
