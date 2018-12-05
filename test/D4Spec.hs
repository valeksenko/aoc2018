module D4Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit

import qualified D4

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D4.idtime1: gives correct answer to the original problem" $ do
            (Just 240) @=? D4.idtime1 [
                        D4.Sleeps (D4.LogTime "1518-11-01" 00 05)
                      , D4.Sleeps (D4.LogTime "1518-11-03" 00 24)
                      , D4.Starts 10 (D4.LogTime "1518-11-01" 00 00)
                      , D4.Wakes (D4.LogTime "1518-11-01" 00 25)
                      , D4.Starts 10 (D4.LogTime "1518-11-03" 00 05)
                      , D4.Sleeps (D4.LogTime "1518-11-05" 00 45)
                      , D4.Sleeps (D4.LogTime "1518-11-02" 00 40)
                      , D4.Sleeps (D4.LogTime "1518-11-01" 00 30)
                      , D4.Wakes (D4.LogTime "1518-11-01" 00 55)
                      , D4.Starts 99 (D4.LogTime "1518-11-01" 23 58)
                      , D4.Wakes (D4.LogTime "1518-11-02" 00 50)
                      , D4.Wakes (D4.LogTime "1518-11-03" 00 29)
                      , D4.Starts 99 (D4.LogTime "1518-11-04" 00 02)
                      , D4.Sleeps (D4.LogTime "1518-11-04" 00 36)
                      , D4.Wakes (D4.LogTime "1518-11-04" 00 46)
                      , D4.Starts 99 (D4.LogTime "1518-11-05" 00 03)
                      , D4.Wakes (D4.LogTime "1518-11-05" 00 55)
                    ]
      , testCase "D4.idtime2: gives correct answer to the original problem" $ do
            (Just 4455) @=? D4.idtime2 [
                        D4.Sleeps (D4.LogTime "1518-11-01" 00 05)
                      , D4.Sleeps (D4.LogTime "1518-11-03" 00 24)
                      , D4.Starts 10 (D4.LogTime "1518-11-01" 00 00)
                      , D4.Wakes (D4.LogTime "1518-11-01" 00 25)
                      , D4.Starts 10 (D4.LogTime "1518-11-03" 00 05)
                      , D4.Sleeps (D4.LogTime "1518-11-05" 00 45)
                      , D4.Sleeps (D4.LogTime "1518-11-02" 00 40)
                      , D4.Sleeps (D4.LogTime "1518-11-01" 00 30)
                      , D4.Wakes (D4.LogTime "1518-11-01" 00 55)
                      , D4.Starts 99 (D4.LogTime "1518-11-01" 23 58)
                      , D4.Wakes (D4.LogTime "1518-11-02" 00 50)
                      , D4.Wakes (D4.LogTime "1518-11-03" 00 29)
                      , D4.Starts 99 (D4.LogTime "1518-11-04" 00 02)
                      , D4.Sleeps (D4.LogTime "1518-11-04" 00 36)
                      , D4.Wakes (D4.LogTime "1518-11-04" 00 46)
                      , D4.Starts 99 (D4.LogTime "1518-11-05" 00 03)
                      , D4.Wakes (D4.LogTime "1518-11-05" 00 55)
                    ]

    ]
