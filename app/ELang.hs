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
  | LambdaAbs String Type Exp Type
  | App Exp Exp Type
  deriving Show

type TypeContext = [(String, Type)]

isVal :: Exp -> Bool
isVal T = True
isVal F = True
isVal Z = True
isVal (LambdaAbs {}) = True
isVal _ = False


typecheck :: TypeContext -> Exp -> Either String Type
typecheck _ T = Right Bool
typecheck _ F = Right Bool
typecheck _ Z = Right Nat
typecheck ctx (IfThenElse t1 t2 t3) = do
  typet1 <- typecheck ctx t1
  if typet1 == Bool then do
    typet2 <- typecheck ctx t2
    typet3 <- typecheck ctx t3
    if typet2 == typet3 then Right typet2 else Left "Branches have different types"
  else Left "Guard is not a boolean"
typecheck ctx (Succ t) = do
  typet <- typecheck ctx t
  if typet == Nat then Right Nat else Left "Argument is not a number"
typecheck ctx (Pred t) = do
  typet <- typecheck ctx t
  if typet == Nat then Right Nat else Left "Argument is not a number"
typecheck ctx (IsZero t) = do
  typet <- typecheck ctx t
  if typet == Nat then Right Bool else Left "Argument is not a number"
typecheck ctx (Var x) =
  case lookup x ctx of
    Just t -> Right t
    Nothing -> Left "Variable not found in context"
typecheck ctx (LambdaAbs x varType t2 termType) = do
  t2Type <- typecheck ((x, varType) : ctx) t2
  Right (Arrow varType t2Type)
typecheck ctx (App t1 t2 termType) = do
  t1Type <- typecheck ctx t1
  t2Type <- typecheck ctx t2
  case t1Type of
    Arrow t11 t12 -> if t11 == t2Type then Right t12 else Left "Argument type mismatch"
    _ -> Left "Function type expected"


freeVariables :: Exp -> [String]
freeVariables (Var x) = [x]
freeVariables (LambdaAbs x _ t _) = filter (/= x) (freeVariables t)
freeVariables (App t1 t2 _) = freeVariables t1 ++ freeVariables t2
freeVariables (IfThenElse t1 t2 t3 ) = freeVariables t1 ++ freeVariables t2 ++ freeVariables t3
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

substitute x s (LambdaAbs y varType t bodyType)
  | x == y = LambdaAbs y varType t bodyType
  | y `elem` freeVariables s =
      let z = freshVariable y (freeVariables s ++ freeVariables t)
      in LambdaAbs z varType (substitute y (Var z) t) bodyType
  | otherwise = LambdaAbs y varType (substitute x s t) bodyType

substitute x s (App t1 t2 appType) = App (substitute x s t1) (substitute x s t2) appType
substitute x s (IfThenElse t1 t2 t3) = IfThenElse (substitute x s t1) (substitute x s t2) (substitute x s t3)
substitute x s (Succ t) = Succ (substitute x s t)
substitute x s (Pred t) = Pred (substitute x s t)
substitute x s (IsZero t) = IsZero (substitute x s t)
substitute _ _ t = t

reduce :: TypeContext -> Exp -> Either String Exp
reduce _ T = Left "Cannot reduce True (it is a value)"
reduce _ F = Left "Cannot reduce False (it is a value)"
reduce _ Z = Left "Cannot reduce Zero (it is a value)"

reduce env (Succ t) = do
  t' <- reduce env t
  Right (Succ t')

reduce env (Pred Z) = Right Z
reduce env (Pred (Succ t)) = Right t
reduce env (Pred t) = do
  t' <- reduce env t
  Right (Pred t')

reduce env (IsZero Z) = Right T
reduce env (IsZero (Succ t )) = Right F
reduce env (IsZero t) = do
  t' <- reduce env t
  Right (IsZero t')

reduce env (IfThenElse T t1 _) = Right t1
reduce env (IfThenElse F _ t2) = Right t2
reduce env (IfThenElse t t1 t2) = do
  t' <- reduce env t
  Right (IfThenElse t' t1 t2)

reduce env (LambdaAbs x paramType body bodyType) = do
  body' <- reduce env body
  Right (LambdaAbs x paramType body' bodyType)

reduce env (App (LambdaAbs x paramType body bodyType) v2 appType)
  | isVal v2 = Right (substitute x v2 body)
reduce env (App t1 t2 appType) | isVal t1 = do
  t2' <- reduce env t2
  Right (App t1 t2' appType)
reduce env (App t1 t2 appType) = do
  t1' <- reduce env t1
  Right (App t1' t2 appType)

reduce _ (Var x) = Left ("Cannot reduce a free variable: " ++ x)

reduceStar :: TypeContext -> Exp -> Either String Exp
reduceStar ctx exp = do
  _ <- typecheck ctx exp
  reduceFully ctx exp

reduceFully :: TypeContext -> Exp -> Either String Exp
reduceFully ctx exp = 
  case reduce ctx exp of
    Left _ -> Right exp
    Right exp' -> reduceFully ctx exp'

customPrint :: Exp -> String
customPrint T = "true"
customPrint F = "false"
customPrint Z = "0"
customPrint (IfThenElse t1 t2 t3) = "if " ++ customPrint t1 ++ " then " ++ customPrint t2 ++ " else " ++ customPrint t3
customPrint (Succ t) = "succ " ++ customPrint t
customPrint (Pred t) = "pred " ++ customPrint t
customPrint (IsZero t) = "iszero " ++ customPrint t
customPrint (Var x) = x
customPrint (LambdaAbs x _ t _) = "/" ++ x ++ "." ++ customPrint t
customPrint (App t1 t2 _) = "(" ++ customPrint t1 ++ ") " ++ customPrint t2


