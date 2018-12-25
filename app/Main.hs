module Main where

import D24P1

main :: IO ()
main = do
    putStrLn . show $ winningunits [
                                Group 0 ImmuneSystem 3578 3874 [Radiation] [] 10 Bludgeoning 17
                              , Group 1 ImmuneSystem 865 10940 [] [Bludgeoning, Cold] 94 Cold 19
                              , Group 2 ImmuneSystem 3088 14516 [Cold] [] 32 Bludgeoning 4
                              , Group 3 ImmuneSystem 2119 6577 [Slashing, Fire] [Cold] 22 Bludgeoning 6
                              , Group 4 ImmuneSystem 90 2089 [Bludgeoning] [] 213 Cold 14
                              , Group 5 ImmuneSystem 1341 4768 [Bludgeoning, Radiation, Cold] [] 34 Bludgeoning 1
                              , Group 6 ImmuneSystem 2846 5321 [Cold] [] 17 Cold 13
                              , Group 7 ImmuneSystem 4727 7721 [] [Radiation] 15 Fire 10
                              , Group 8 ImmuneSystem 1113 11891 [Cold] [Fire] 80 Fire 18
                              , Group 9 ImmuneSystem 887 5712 [] [Bludgeoning] 55 Slashing 15
                              , Group 10 Infection 3689 32043 [Slashing] [Cold, Fire] 16 Cold 7
                              , Group 11 Infection 33 10879 [] [Slashing] 588 Slashing 12
                              , Group 12 Infection 2026 49122 [] [Bludgeoning] 46 Fire 16
                              , Group 13 Infection 7199 9010 [Radiation, Bludgeoning] [Slashing] 2 Slashing 8
                              , Group 14 Infection 2321 35348 [] [Cold] 29 Radiation 20
                              , Group 15 Infection 484 21952 [] [] 84 Radiation 9
                              , Group 16 Infection 2531 24340 [] [] 18 Fire 3
                              , Group 17 Infection 54 31919 [Bludgeoning, Cold] [] 1178 Radiation 5
                              , Group 18 Infection 1137 8211 [Slashing, Radiation, Bludgeoning] [Cold] 14 Bludgeoning 11
                              , Group 19 Infection 2804 17948 [] [] 11 Radiation 2                          
                        ]
