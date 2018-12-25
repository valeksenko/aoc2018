module D25P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D25P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D25P1.constellations: gives correct answer to the original problem" $ do
            2 @=? constellations [
                        (0,0,0,0), (3,0,0,0), (0,3,0,0), (0,0,3,0), (0,0,0,3), (0,0,0,6), (9,0,0,0), (12,0,0,0)
                    ]
            3 @=? constellations [
                        (1,-1,0,1), (2,0,-1,0), (3,2,-1,0), (0,0,3,1), (0,0,-1,-1), (2,3,-2,0), (-2,2,0,0), (2,-2,0,-1), (1,-1,0,-1), (3,2,0,2)
                    ]
            8 @=? constellations [
                        (1,-1,-1,-2), (-2,-2,0,1), (0,2,1,3), (-2,3,-2,1), (0,2,3,-2), (-1,-1,1,-2), (0,-2,-1,0), (-2,2,3,-1), (1,2,2,0), (-1,-2,0,-2)
                    ]
    ]
