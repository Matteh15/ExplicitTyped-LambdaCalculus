module ELang where

data Type = Bool | Nat | Arrow Type Type deriving (Eq, Show)

data Exp =
  T
  | F 
  | Z 
  | IfThenElse Exp Exp Exp 
  | Succ Exp
  | Pred Exp 
  | IsZero Exp 
  | Var String
  | LambdaAbs String Exp
  | App Exp Exp
  deriving Show

isNum :: Exp -> Bool
isNum Z     = True
isNum (Succ n) = isNum n
isNum _        = False

isVal :: Exp -> Bool
isVal T = True
isVal F = True
isVal (LambdaAbs _ _) = True
isVal t = isNum t

freeVariables :: Exp -> [String]
freeVariables (Var x) = [x]
freeVariables (LambdaAbs x t) = filter (/= x) (freeVariables t)
freeVariables (App t1 t2) = freeVariables t1 ++ freeVariables t2
freeVariables (IfThenElse t1 t2 t3) = freeVariables t1 ++ freeVariables t2 ++ freeVariables t3
freeVariables (Succ t) = freeVariables t
freeVariables (Pred t) = freeVariables t
freeVariables (IsZero t) = freeVariables t
freeVariables _ = []

freshVariable :: String -> [String] -> String
freshVariable x xs =
  let candidates = [x ++ show i | i <- [1..]]
  in head (filter (`notElem` xs) candidates)

substitute :: String -> Exp -> Exp -> Exp
substitute x s (Var y) 
  | x == y = s
  | otherwise = Var y
substitute x s (LambdaAbs y t)
  | x == y = LambdaAbs y t
  | y `elem` freeVariables s = 
    let y' = freshVariable y (freeVariables s ++ freeVariables t)
    in LambdaAbs y' (substitute x s (substitute y (Var y') t))
  | otherwise = LambdaAbs y (substitute x s t)
substitute x s (App t1 t2) = App (substitute x s t1) (substitute x s t2)
substitute x s (IfThenElse t1 t2 t3) = IfThenElse (substitute x s t1) (substitute x s t2) (substitute x s t3)
substitute x s (Succ t) = Succ (substitute x s t)
substitute x s (Pred t) = Pred (substitute x s t)
substitute x s (IsZero t) = IsZero (substitute x s t)
substitute _ _ t = t

reduce :: Exp -> Maybe Exp
reduce T = Nothing
reduce F = Nothing
reduce Z = Nothing

reduce (IfThenElse T t1 _)  = return t1
reduce (IfThenElse F _ t2)  = return t2
reduce (IfThenElse t t1 t2) = do 
  t' <- reduce t
  return (IfThenElse t' t1 t2)

reduce (Succ t) = do
 t' <- reduce t
 return (Succ t')

reduce (Pred Z) = return Z
reduce (Pred (Succ n)) | isNum n = return n
reduce (Pred t) = do
 t' <- reduce t
 return (Pred t')

reduce (IsZero Z) = return T
reduce (IsZero (Succ t)) | isNum t = return F
reduce (IsZero t) = do
 t' <- reduce t
 return (IsZero t')

reduce (App (LambdaAbs x t1) v2) | isVal v2 = return (substitute x v2 t1)
reduce (App t1 t2) | isVal t1 = do
 t2' <- reduce t2
 return (App t1 t2')
reduce (App t1 t2) = do
  t1' <- reduce t1
  return (App t1' t2)

reduce (LambdaAbs x t) = do
  t' <- reduce t
  return (LambdaAbs x t')

reduce (Var x) = Nothing


reduceStar :: Exp -> Exp
reduceStar t = 
  let r = reduce t in
    case r of
      Just t' -> reduceStar t'
      _       -> t

customPrint :: Exp -> String
customPrint T = "true"
customPrint F = "false"
customPrint Z = "0"
customPrint (IfThenElse t1 t2 t3) = "if " ++ customPrint t1 ++ " then " ++ customPrint t2 ++ " else " ++ customPrint t3
customPrint (Succ t) = "succ " ++ customPrint t
customPrint (Pred t) = "pred " ++ customPrint t
customPrint (IsZero t) = "iszero " ++ customPrint t
customPrint (Var x) = x
customPrint (LambdaAbs x t) = "/" ++ x ++ "." ++ customPrint t 
customPrint (App t1 t2) = "(" ++ customPrint t1 ++ ") " ++ customPrint t2
