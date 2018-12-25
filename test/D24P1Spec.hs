module D24P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D24P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D24P1.winningunits: gives correct answer to the original problem" $ do
            5216 @=? winningunits [
                        Group 0 ImmuneSystem 17 5390 [] [Radiation, Bludgeoning] 4507 Fire 2
                      , Group 1 ImmuneSystem 989 1274 [Fire] [Slashing, Bludgeoning] 25 Slashing 3
                      , Group 3 Infection 801 4706 [] [Radiation] 116 Bludgeoning 1
                      , Group 4 Infection 4485 2961 [Radiation] [Fire, Cold] 12 Slashing 4
                    ]
    ]