module D2P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D2P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D2P1.checksum: gives correct answer to the original problem" $ do
            12 @=? D2P1.checksum ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
    ]

