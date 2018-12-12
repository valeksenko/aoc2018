module D11P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D11P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D11P2.maxpower: gives correct answer to the original problem" $ do
            (((90, 269), 100), 16)  @=? D11P2.maxpower 18
            (((232, 251), 100), 42)  @=? D11P2.maxpower 42
            (((233,116), 200), 15)  @=? D11P2.maxpower 9995
    ]