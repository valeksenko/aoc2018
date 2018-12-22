module D20P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import D20P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D20P1.dooramount: gives correct answer to the original problem" $ do
            3 @=? dooramount "^WNE$"
            10 @=? dooramount "^ENWWW(NEEE|SSE(EE|N))$"
            18 @=? dooramount "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
            23 @=? dooramount "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
            31 @=? dooramount "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"
    ]