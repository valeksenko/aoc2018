module D24P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D24
import D24P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D24P2.boostwinningunits: gives correct answer to the original problem" $ do
            51 @=? boostwinningunits [
                        Group 0 ImmuneSystem 17 5390 [] [Radiation, Bludgeoning] 4507 Fire 2
                      , Group 1 ImmuneSystem 989 1274 [Fire] [Slashing, Bludgeoning] 25 Slashing 3
                      , Group 3 Infection 801 4706 [] [Radiation] 116 Bludgeoning 1
                      , Group 4 Infection 4485 2961 [Radiation] [Fire, Cold] 12 Slashing 4
                    ]
    ]