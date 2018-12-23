module D23P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D23P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D23P1.inrangenanobots: gives correct answer to the original problem" $ do
            7 @=? inrangenanobots [((0,0,0), 4), ((1,0,0), 1), ((4,0,0), 3), ((0,2,0), 1), ((0,5,0), 3), ((0,0,3), 1), ((1,1,1), 1), ((1,1,2), 1), ((1,3,1), 1)]
    ]