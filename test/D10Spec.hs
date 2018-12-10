module D10Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D10

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D10.message: gives correct answer to the original problem" $ do
            3 @=? (
                fst $ D10.message 1 [
                        (0,2),(-1,0),(-1,1),(-2,-1),(2,2),(2,-2),(1,-1),(1,0),(1,-2),(-1,-1),(1,0),(2,0),(-1,1),(1,-2),(0,-1),(0,1),(-2,0),(1,0),(0,-1),(-1,1),(0,-1),(2,0),(1,2),(2,1),(2,-2),(-1,-1),(1,0),(2,0),(1,-2),(-2,0),(2,-1)
                    ] [
                        (9,1),(7,0),(3,-2),(6,10),(2,-4),(-6,10),(1,8),(1,7),(-3,11),(7,6),(-2,3),(-4,3),(10,-3),(5,11),(4,7),(8,-2),(15,0),(1,6),(8,9),(3,3),(0,5),(-2,2),(5,-2),(1,4),(-2,7),(3,6),(5,0),(-6,0),(5,9),(14,7),(-3,6)
                    ]
                )

    ]
