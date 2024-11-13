module Main where

import ELang

main :: IO ()
main = do
    let t = reduceStar (IfThenElse Tru (Succ Zero) Zero)
    print t