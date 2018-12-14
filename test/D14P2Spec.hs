module D14P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D14P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D14P2.recipecount: gives correct answer to the original problem" $ do
            9 @=? recipecount [5,1,5,8,9] [3,7]
            5 @=? recipecount [0,1,2,4,5] [3,7]
            18 @=? recipecount [9,2,5,1,0] [3,7]
            2018 @=? recipecount [5,9,4,1,4] [3,7]
    ]