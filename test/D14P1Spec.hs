module D14P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D14P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D14P1.recipescore: gives correct answer to the original problem" $ do
            [0,1,2,4,5,1,5,8,9,1] @=? recipescore 5 [3,7]
            [5,1,5,8,9,1,6,7,7,9] @=? recipescore 9 [3,7]
            [5,9,4,1,4,2,9,8,8,2] @=? recipescore 2018 [3,7]
            [1,0,4,4,2,5,7,3,9,7] @=? recipescore 503761 [3,7]
    ]