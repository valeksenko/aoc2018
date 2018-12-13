module Main where

import D12

main :: IO ()
main = do
    putStrLn . show $ D12.potsum [True, False, False, False, True, True, True, True, True, False, True, False, False, True, True, False, False, False, True, True, False, False, False, True, False, True, True, False, True, False, True, True, False, True, True, True, False, False, True, True, False, True, True, False, True, False, True, False, False, True, False, False, False, True, True, True, False, False, True, True, True, True, False, True, False, False, False, False, False, True, False, False, True, True, False, False, True, False, True, True, False, False, False, False, False, False, True, True, True, True, True, False, False, True, True, True, True, False, False, False] 50000000000 [
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
