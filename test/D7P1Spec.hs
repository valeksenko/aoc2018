module D7P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D7P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D7P1.steporder: gives correct answer to the original problem" $ do
            "CABDFE" @=? D7P1.steporder [('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]
    ]
