module Main where

import ELang

main :: IO ()
main = do
    let exp1 = Succ Z                                       -- 1
    let exp2 = Pred (Succ Z)                                -- 0
    let exp3 = IsZero (Pred (Succ Z))                       -- True
    let exp4 = IfThenElse T (Succ Z) (Pred (Succ Z))        -- 1
    let exp5 = IfThenElse F (Succ Z) (Pred (Succ Z))        -- 0
    let exp6 = LambdaAbs "x" Nat $ Var "x"                  -- λx:Nat.x
    let exp7 = App (LambdaAbs "x" Nat $ Var "x") (Succ Z)   -- 1
    let exp8 = App (LambdaAbs "x" Nat $ Var "x") T          -- Type error
    let exp9 = LambdaAbs "x" Nat $ 
                LambdaAbs "y" Nat $ Var "x"                 -- λx:Nat.λy:Nat.x
    let exp10 = 
            LambdaAbs "x" Bool $
                LambdaAbs "y" Nat $
                    LambdaAbs "z" Nat $
                        IfThenElse (Var "x") (Var "y") (Var "z") -- λx:Bool.λy:Nat.λz:Nat.if x then y else z

    let resExp1 = reduceStar [] exp1
    case resExp1 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp2 = reduceStar [] exp2
    case resExp2 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp3 = reduceStar [] exp3
    case resExp3 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp4 = reduceStar [] exp4
    case resExp4 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp5 = reduceStar [] exp5
    case resExp5 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp6 = reduceStar [] exp6
    case resExp6 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp7 = reduceStar [] exp7
    case resExp7 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp8 = reduceStar [] exp8
    case resExp8 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp9 = reduceStar [] $ App (App exp9 (Succ (Succ Z))) Z
    case resExp9 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp

    let resExp10 = reduceStar [] $ App (App (App exp10 $ IsZero $ Succ Z) (Succ Z)) Z
    case resExp10 of
        Left err -> putStrLn $ "Error: " ++ err
        Right exp -> putStrLn $ "Result: " ++ show exp