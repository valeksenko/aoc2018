module D17P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Vector as V

import D17P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D17P2.drainedcount: gives correct answer to the original problem" $ do
            29 @=? drainedcount [
                            (2,495),(3,495),(4,495),(5,495),(6,495),(7,495),(7,495),(7,496),(7,497),(7,498),(7,499),(7,500),(7,501),(3,501),(4,501),(5,501),(6,501),(7,501),(2,498),(3,498),(4,498),(1,506),(2,506),(10,498),(11,498),(12,498),(13,498),(10,504),(11,504),(12,504),(13,504),(13,498),(13,499),(13,500),(13,501),(13,502),(13,503),(13,504)
                        ]
    ]