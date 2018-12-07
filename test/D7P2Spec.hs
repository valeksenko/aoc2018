module D7P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D7P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D7P2.totaltime: gives correct answer to the original problem" $ do
            15 @=? D7P2.totaltime 2 0 [('C', 'A'), ('C', 'F'), ('A', 'B'), ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]
    ]
