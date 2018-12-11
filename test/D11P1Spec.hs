module D11P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D11P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D11P1.powercell: gives correct answer to the original problem" $ do
            (33,45)  @=? D11P1.powersquare 18
            (21,61)  @=? D11P1.powersquare 42
            (33,45)  @=? D11P1.powersquare 9995
    ]