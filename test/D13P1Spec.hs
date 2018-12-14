module D13P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D13P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D13P1.firstcrash: gives correct answer to the original problem" $ do
            (7,3)  @=? firstcrash [
                        Curve LeftUp (0,0), Horizontal (1,0), Horizontal (2,0), Horizontal (3,0), Curve RightUp (4,0)
                      , Vertical (0,1), Vertical (4,1), Curve LeftUp (7,1), Horizontal (8,1), Horizontal (9,1), Horizontal (10,1), Horizontal (11,1), Curve RightUp (12,1)
                      , Vertical (0,2), Curve LeftUp (2,2), Horizontal (3,2), Intersection (4,2), Horizontal (5,2), Horizontal (6,2), Intersection (7,2), Horizontal (8,2), Curve RightUp (9,2), Vertical (12,2)
                      , Vertical (0,3), Vertical (2,3), Vertical (4,3), Vertical (7,3), Vertical (9,3), Vertical (12,3)
                      , Curve LeftDown (0,4), Horizontal (1,4), Intersection (2,4), Horizontal (3,4), Curve RightDown (4,4), Curve LeftDown (7,4), Horizontal (8,4), Intersection (9,4), Horizontal (10,4), Horizontal (11,4), Curve RightDown (12,4)
                      , Curve LeftDown (2,5), Horizontal (3,5), Horizontal (4,5), Horizontal (5,5), Horizontal (6,5), Horizontal (7,5), Horizontal (8,5), Curve RightDown (9,5)
                    ] [
                        Car (2,0) RightWard [LeftTurn, GoStraight, RightTurn]
                      , Car (9,3) DownWard [LeftTurn, GoStraight, RightTurn]
                    ] 
    ]