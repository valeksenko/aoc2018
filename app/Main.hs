module Main where

import Control.Applicative
import Z3.Monad

script :: Z3 Result
script = do
    a <- mkFreshIntVar "a"
    _10 <- mkInteger 10
    _100 <- mkInteger 100
    true <- mkTrue

    intSort <- mkIntSort
    boolSort <- mkBoolSort
    fDecl <- mkFreshFuncDecl "f" [intSort, boolSort] intSort
    
    assert =<< mkGt a _10

    r <- mkApp fDecl [a, true]
    assert =<< mkLt r _100

    (res, mbModel) <- getModel
    return res
    -- case mbModel of
    --     Just model -> do
    --         Just f <- evalFunc model fDecl
    --         toRetType f
    --     Nothing -> error ("Couldn't construct model: " ++ show res)

main :: IO ()
main = do
    evalZ3 script >>= print
