module D18P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D18P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D18P1.lumbervalue: gives correct answer to the original problem" $ do
            1147 @=? lumbervalue [
                            (Open,(0,0)),(Lumberyard,(0,1)),(Open,(0,2)),(Lumberyard,(0,3)),(Open,(0,4)),(Open,(0,5)),(Open,(0,6)),(Trees,(0,7)),(Lumberyard,(0,8)),(Open,(0,9)),(Open,(1,0)),(Open,(1,1)),(Open,(1,2)),(Open,(1,3)),(Open,(1,4)),(Lumberyard,(1,5)),(Trees,(1,6)),(Lumberyard,(1,7)),(Lumberyard,(1,8)),(Trees,(1,9)),(Open,(2,0)),(Trees,(2,1)),(Open,(2,2)),(Open,(2,3)),(Trees,(2,4)),(Open,(2,5)),(Open,(2,6)),(Open,(2,7)),(Lumberyard,(2,8)),(Open,(2,9)),(Open,(3,0)),(Open,(3,1)),(Trees,(3,2)),(Lumberyard,(3,3)),(Open,(3,4)),(Open,(3,5)),(Open,(3,6)),(Open,(3,7)),(Open,(3,8)),(Lumberyard,(3,9)),(Lumberyard,(4,0)),(Open,(4,1)),(Lumberyard,(4,2)),(Trees,(4,3)),(Trees,(4,4)),(Trees,(4,5)),(Lumberyard,(4,6)),(Trees,(4,7)),(Lumberyard,(4,8)),(Trees,(4,9)),(Open,(5,0)),(Open,(5,1)),(Open,(5,2)),(Lumberyard,(5,3)),(Open,(5,4)),(Trees,(5,5)),(Trees,(5,6)),(Open,(5,7)),(Open,(5,8)),(Open,(5,9)),(Open,(6,0)),(Trees,(6,1)),(Open,(6,2)),(Open,(6,3)),(Open,(6,4)),(Open,(6,5)),(Trees,(6,6)),(Open,(6,7)),(Open,(6,8)),(Open,(6,9)),(Trees,(7,0)),(Trees,(7,1)),(Open,(7,2)),(Open,(7,3)),(Open,(7,4)),(Lumberyard,(7,5)),(Trees,(7,6)),(Open,(7,7)),(Lumberyard,(7,8)),(Trees,(7,9)),(Trees,(8,0)),(Open,(8,1)),(Trees,(8,2)),(Trees,(8,3)),(Trees,(8,4)),(Trees,(8,5)),(Open,(8,6)),(Open,(8,7)),(Trees,(8,8)),(Open,(8,9)),(Open,(9,0)),(Open,(9,1)),(Open,(9,2)),(Lumberyard,(9,3)),(Open,(9,4)),(Trees,(9,5)),(Open,(9,6)),(Open,(9,7)),(Trees,(9,8)),(Open,(9,9))
                        ]
    ]