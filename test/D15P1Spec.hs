module D15P1Spec (
    tests
) where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Sequence as S

import D15P1

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [
        testCase "D15P1.battlescore: gives correct answer to the original problem" $ do
            -- 27730 @=? battlescore (
            --             S.fromList [(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,2),(2,3),(2,4),(2,5),(3,1),(3,3),(3,5),(4,1),(4,2),(4,3),(4,5),(5,1),(5,2),(5,3),(5,4),(5,5)]
            --         ) (
            --             S.fromList [Npc {nType = Goblin, nPos = (1,2), nHP = 200},Npc {nType = Elf, nPos = (2,4), nHP = 200},Npc {nType = Goblin, nPos = (2,5), nHP = 200},Npc {nType = Goblin, nPos = (3,5), nHP = 200},Npc {nType = Goblin, nPos = (4,3), nHP = 200},Npc {nType = Elf, nPos = (4,5), nHP = 200}]
            --         )
            36334 @=? battlescore (
                        S.fromList [(1,1),(1,2),(1,3),(1,5),(2,1),(2,3),(2,4),(2,5),(3,1),(3,2),(3,5),(4,1),(4,2),(4,3),(4,5),(5,1),(5,2),(5,3),(5,4),(5,5)]
                    ) (
                        S.fromList [Npc {nType = Goblin, nPos = (1,1), nHP = 200},Npc {nType = Elf, nPos = (1,5), nHP = 200},Npc {nType = Elf, nPos = (2,1), nHP = 200},Npc {nType = Elf, nPos = (2,3), nHP = 200},Npc {nType = Elf, nPos = (2,5), nHP = 200},Npc {nType = Goblin, nPos = (3,1), nHP = 200},Npc {nType = Elf, nPos = (4,5), nHP = 200},Npc {nType = Elf, nPos = (5,4), nHP = 200}]
                    )
            39514 @=? battlescore (
                        S.fromList [(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,3),(2,4),(2,5),(3,1),(3,2),(3,5),(4,1),(4,2),(4,3),(4,5),(5,1),(5,2),(5,3),(5,5)]
                    ) (
                        S.fromList [Npc {nType = Elf, nPos = (1,1), nHP = 200},Npc {nType = Elf, nPos = (1,4), nHP = 200},Npc {nType = Goblin, nPos = (1,5), nHP = 200},Npc {nType = Goblin, nPos = (2,3), nHP = 200},Npc {nType = Elf, nPos = (2,5), nHP = 200},Npc {nType = Elf, nPos = (3,1), nHP = 200},Npc {nType = Elf, nPos = (3,5), nHP = 200},Npc {nType = Goblin, nPos = (4,1), nHP = 200},Npc {nType = Elf, nPos = (5,3), nHP = 200}]
                    )
            27755 @=? battlescore (
                        S.fromList [(1,1),(1,2),(1,3),(1,5),(2,1),(2,3),(2,4),(2,5),(3,1),(3,2),(3,4),(3,5),(4,1),(4,2),(4,3),(4,5),(5,1),(5,2),(5,3),(5,4),(5,5)]
                    ) (
                        S.fromList [Npc {nType = Elf, nPos = (1,1), nHP = 200},Npc {nType = Goblin, nPos = (1,3), nHP = 200},Npc {nType = Goblin, nPos = (2,3), nHP = 200},Npc {nType = Goblin, nPos = (3,1), nHP = 200},Npc {nType = Goblin, nPos = (3,5), nHP = 200},Npc {nType = Goblin, nPos = (4,1), nHP = 200},Npc {nType = Elf, nPos = (5,4), nHP = 200}]
                    )
            28944 @=? battlescore (
                        S.fromList [(1,1),(1,2),(1,3),(1,4),(1,5),(2,1),(2,3),(2,4),(2,5),(3,1),(3,5),(4,1),(4,3),(4,5),(5,1),(5,2),(5,3),(5,5)]
                    ) (
                        S.fromList [Npc {nType = Elf, nPos = (1,2), nHP = 200},Npc {nType = Goblin, nPos = (2,5), nHP = 200},Npc {nType = Elf, nPos = (4,1), nHP = 200},Npc {nType = Goblin, nPos = (4,3), nHP = 200},Npc {nType = Goblin, nPos = (4,5), nHP = 200},Npc {nType = Goblin, nPos = (5,5), nHP = 200}]
                    )
            18740 @=? battlescore (
                        S.fromList [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(2,1),(2,2),(2,3),(2,5),(2,6),(2,7),(3,1),(3,2),(3,5),(3,6),(3,7),(4,1),(4,2),(4,3),(4,6),(4,7),(5,1),(5,2),(5,3),(5,5),(5,6),(5,7),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7)]
                    ) (
                        S.fromList [Npc {nType = Goblin, nPos = (1,1), nHP = 200},Npc {nType = Elf, nPos = (2,2), nHP = 200},Npc {nType = Goblin, nPos = (3,7), nHP = 200},Npc {nType = Goblin, nPos = (6,2), nHP = 200},Npc {nType = Goblin, nPos = (6,6), nHP = 200},Npc {nType = Goblin, nPos = (7,6), nHP = 200}]
                    )
    ]