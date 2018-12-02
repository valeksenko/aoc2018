module D2P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D2P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D2P2.letters: gives correct answer to the original problem" $ do
            (Just "fgij") @=? D2P2.letters ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
    ]
