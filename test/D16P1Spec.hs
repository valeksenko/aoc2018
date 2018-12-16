module D16P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Sequence as S

import D16P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D16P1.samplecount: gives correct answer to the original problem" $ do
            1 @=? samplecount [([3, 2, 1, 1], (2, 1, 2), [3, 2, 2, 1])]
    ]