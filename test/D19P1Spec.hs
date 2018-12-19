module D19P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Sequence as S

import D16
import D19P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D19P1.backgroundprog: gives correct answer to the original problem" $ do
            7 @=? backgroundprog (0, [
                    SETi (5, 0, 1), SETi (6, 0, 2), ADDi (0, 1, 0), ADDr (1, 2, 3), SETr (1, 0, 0), SETi (8, 0, 4), SETi (9, 0, 5)
                ])
    ]