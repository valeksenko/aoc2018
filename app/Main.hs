module Main where

import D12P1

main :: IO ()
main = do
    putStrLn . show $ D12P1.potsum [True, False, False, False, True, True, True, True, True, False, True, False, False, True, True, False, False, False, True, True, False, False, False, True, False, True, True, False, True, False, True, True, False, True, True, True, False, False, True, True, False, True, True, False, True, False, True, False, False, True, False, False, False, True, True, True, False, False, True, True, True, True, False, True, False, False, False, False, False, True, False, False, True, True, False, False, True, False, True, True, False, False, False, False, False, False, True, True, True, True, True, False, False, True, True, True, True, False, False, False] 20 [
            ([True, False, True, False, True], True),
            ([False, False, True, True, True], False),
            ([True, False, False, True, False], True),
            ([False, True, False, False, False], True),
            ([False, False, True, True, False], True),
            ([True, True, False, True, False], True),
            ([True, True, False, False, True], True),
            ([True, True, True, True, False], True),
            ([False, False, False, True, False], True),
            ([False, False, True, False, True], True),
            ([False, True, True, True, True], True),
            ([True, False, True, True, True], False),
            ([False, False, False, True, True], False),
            ([False, False, True, False, False], False),
            ([True, False, False, False, True], False),
            ([False, True, True, True, False], True),
            ([False, True, False, True, True], False),
            ([False, True, True, False, False], True),
            ([False, False, False, False, True], False),
            ([True, False, False, True, True], False),
            ([True, True, False, True, True], True),
            ([True, False, True, True, False], False),
            ([True, False, False, False, False], False),
            ([True, True, False, False, False], True),
            ([False, True, False, True, False], False),
            ([True, True, True, False, True], True),
            ([True, True, True, True, True], True),
            ([True, False, True, False, False], False),
            ([False, False, False, False, False], False),
            ([False, True, True, False, True], False),
            ([True, True, True, False, False], False),
            ([False, True, False, False, True], False)
        ]
    putStrLn "Done"
