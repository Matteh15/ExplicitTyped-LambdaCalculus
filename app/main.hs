module Main where

import ELang

main :: IO ()
main = do
    let f0 = LambdaAbs "x" (LambdaAbs "y" (IfThenElse (IsZero (Var "x")) (Var "y") F))
    let f1 = App f0 $ Succ Z
    let f2 = LambdaAbs "x" (Var "x")
    let t = reduceStar $ App f2 T
    print t