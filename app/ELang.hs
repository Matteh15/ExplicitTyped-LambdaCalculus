module ELang where

data Exp = Tru
        | Fls 
        | IfThenElse Exp Exp Exp 
        | Zero 
        | Succ Exp
        | Pred Exp 
        | IsZero Exp 
    deriving Show

isNum :: Exp -> Bool
isNum Zero     = True
isNum (Succ n) = isNum n
isNum _        = False

isVal :: Exp -> Bool
isVal Tru = True
isVal Fls = True
isVal t   = isNum t

reduce :: Exp -> Maybe Exp
reduce Tru = Nothing
reduce Fls = Nothing
reduce (IfThenElse Tru t1 _) = return t1
reduce (IfThenElse Fls _ t2) = return t2
reduce (IfThenElse t t1 t2)  = do 
  t' <- reduce t
  return (IfThenElse t' t1 t2)
reduce Zero = Nothing
reduce (Succ t) = do
 t' <- reduce t
 return (Succ t')
reduce (Pred Zero)               = return Zero
reduce (Pred (Succ n)) | isNum n = return n
reduce (Pred t) = do
 t' <- reduce t
 return (Pred t')
reduce (IsZero Zero)               = return Tru
reduce (IsZero (Succ t)) | isNum t = return Fls
reduce (IsZero t) = do
 t' <- reduce t
 return (IsZero t')

reduceStar :: Exp -> Exp
reduceStar t = 
  let r = reduce t in
    case r of
      Just t' -> reduceStar t'
      _       -> t

t = reduceStar (IfThenElse (IsZero (Succ (Pred Zero))) (Pred (Succ Zero)) Zero)