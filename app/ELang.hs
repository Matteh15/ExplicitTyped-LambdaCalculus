module ELang where

data Exp =
  T
  | F 
  | Z 
  | IfThenElse Exp Exp Exp 
  | Succ Exp
  | Pred Exp 
  | IsZero Exp 
  deriving Show

isNum :: Exp -> Bool
isNum Z     = True
isNum (Succ n) = isNum n
isNum _        = False

isVal :: Exp -> Bool
isVal T = True
isVal F = True
isVal t = isNum t

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

reduceStar :: Exp -> Exp
reduceStar t = 
  let r = reduce t in
    case r of
      Just t' -> reduceStar t'
      _       -> t

t = reduceStar (IfThenElse (IsZero (Succ (Pred Z))) (Pred (Succ Z)) Z)