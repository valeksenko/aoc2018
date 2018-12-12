module D12P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D12P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D12P1.potsum: gives correct answer to the original problem" $ do
            325 @=? D12P1.potsum [True, False, False, True, False, True, False, False, True, True, False, False, False, False, False, False, True, True, True, False, False, False, True, True, True] 20 [
                    ([False, False, False, True, True], True),
                    ([False, False, True, False, False], True),
                    ([False, True, False, False, False], True),
                    ([False, True, False, True, False], True),
                    ([False, True, False, True, True], True),
                    ([False, True, True, False, False], True),
                    ([False, True, True, True, True], True),
                    ([True, False, True, False, True], True),
                    ([True, False, True, True, True], True),
                    ([True, True, False, True, False], True),
                    ([True, True, False, True, True], True),
                    ([True, True, True, False, False], True),
                    ([True, True, True, False, True], True),
                    ([True, True, True, True, False], True)
                ]

    ]