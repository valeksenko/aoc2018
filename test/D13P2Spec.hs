module D13P2Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D13
import D13P2

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D13P2.lastcar: gives correct answer to the original problem" $ do
            (6,4)  @=? lastcar [
                            Curve LeftUp (0,0),Horizontal (1,0),Horizontal (2,0),Horizontal (3,0),Curve RightUp (4,0),Vertical (0,1),Vertical (4,1),Vertical (0,2),Curve LeftUp (2,2),Horizontal (3,2),Intersection (4,2),Horizontal (5,2),Curve RightUp (6,2),Vertical (0,3),Vertical (2,3),Vertical (4,3),Vertical (6,3),Curve LeftDown (0,4),Horizontal (1,4),Intersection (2,4),Horizontal (3,4),Curve RightDown (4,4),Vertical (6,4),Vertical (2,5),Vertical (6,5),Curve LeftDown (2,6),Horizontal (3,6),Horizontal (4,6),Horizontal (5,6),Curve RightDown (6,6)
                        ] [
                            Car {cPos = (1,0), cDir = RightWard, cTurns = [LeftTurn,GoStraight,RightTurn]},Car {cPos = (3,0), cDir = LeftWard, cTurns = [LeftTurn,GoStraight,RightTurn]},Car {cPos = (3,2), cDir = LeftWard, cTurns = [LeftTurn,GoStraight,RightTurn]},Car {cPos = (6,3), cDir = DownWard, cTurns = [LeftTurn,GoStraight,RightTurn]},Car {cPos = (1,4), cDir = RightWard, cTurns = [LeftTurn,GoStraight,RightTurn]},Car {cPos = (3,4), cDir = LeftWard, cTurns = [LeftTurn,GoStraight,RightTurn]},Car {cPos = (6,5), cDir = UpWard, cTurns = [LeftTurn,GoStraight,RightTurn]},Car {cPos = (3,6), cDir = LeftWard, cTurns = [LeftTurn,GoStraight,RightTurn]},Car {cPos = (5,6), cDir = RightWard, cTurns = [LeftTurn,GoStraight,RightTurn]}
                        ]
    ]