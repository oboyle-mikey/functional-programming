{- oboylemi Michael O'Boyle -}
module Ex01 where
import Data.List ((\\))
import Data.Maybe

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Eq k => Dict k d -> k -> Maybe d
find []             _                 =  Nothing
find ( (s,v) : ds ) name | name == s  =  Just v
                         | otherwise  =  find ds name

type EDict = Dict String Double

-- Part 1 : Evaluating Expressions -- (63 marks) -------------------------------

-- Implement the following function so all 'eval' tests pass.

-- eval should return Nothing if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.

eval :: EDict -> Expr -> Maybe Double
eval _ (Val x) = Just x
eval d (Val x) = Just x
eval d (Var i) = find d i
eval _ (Var i) = Nothing

eval d (Mul x y)
 =case (eval d x, eval d y) of
   (Just m , Just n) -> Just (m*n)
   _                 -> Nothing

eval d (Add x y)
 =case (eval d x, eval d y) of
   (Just m , Just n) -> Just (m+n)
   _                 -> Nothing

eval d (Sub x y)
 =case (eval d x, eval d y) of
   (Just m , Just n) -> Just (m-n)
   _                 -> Nothing

eval d (Dvd x y)
 =case (eval d x, eval d y) of
   (Just m, Just n)
     -> if n==0.0 then Nothing else Just (m/n)
   _ -> Nothing

eval d (Def x e1 e2)
 =case (eval d e1) of
  Nothing -> Nothing
  Just v1 -> eval (define d x v1) e2


-- Part 2 : Simplifying Expressions -- (57 marks) ------------------------------

-- Given the following code :

simp :: EDict -> Expr -> Expr
simp d (Var v)        =  simpVar d v
simp d (Add e1 e2)    =  simpAdd d   (simp d e1) (simp d e2)
simp d (Sub e1 e2)    =  simpSub d   (simp d e1) (simp d e2)
simp d (Mul e1 e2)    =  simpMul d   (simp d e1) (simp d e2)
simp d (Dvd e1 e2)    =  simpDvd d   (simp d e1) (simp d e2)
simp d (Def v e1 e2)  =  simpDef d v (simp d e1) (simp d e2)
simp _ e = e  -- simplest case, Val, needs no special treatment

-- Implement the following functions so all 'simp' tests pass.

  -- (1) see test scripts for most required properties
  -- (2) (Def v e1 e2) should simplify to e2,...
    -- (2a) .... if there is no  mention of v in e2
    -- (2b) .... or any mention of v in e2 is inside another (Def v .. ..)

simpVar :: EDict -> Id -> Expr
simpVar d i = case find d i of
 Nothing -> Var i
 Just a -> Val a

simpAdd :: EDict -> Expr -> Expr -> Expr
simpAdd d e1 e2 = case (eval d e1, eval d e2) of
 (Just 0.0, _) -> e2
 (_, Just 0.0) -> e1
 (Just m, Just n) -> Val(m+n)
 (_,_) -> Add e1 e2

simpSub :: EDict -> Expr -> Expr -> Expr
simpSub d e1 e2 = case (eval d e1, eval d e2) of
 (_, Just 0.0) -> e1
 (Just m, Just n) -> Val(m-n)
 (_,_) -> Sub e1 e2



simpMul :: EDict -> Expr -> Expr -> Expr
simpMul d e1 e2
 = let
  i = simp d e1
  j = simp d e2
 in case (i, j) of
  (e, Val 1.0) -> e
  (Val 1.0, e) -> e
  (_, Val 0.0) -> Val 0.0
  (Val 0.0, _) -> Val 0.0
  (Val i,Val j) -> Val(i*j)
  
 

simpDvd :: EDict -> Expr -> Expr -> Expr
simpDvd d e1 e2 = case (eval d e1, eval d e2) of
 (_, Just 0.0) -> Dvd e1 (Val 0.0)
 (Just 0.0, _) -> Val 0.0
 (Just m, Just 1.0) -> Val m 
 (Just m, Just n) -> Val(m/n)
 (_,_) -> Dvd e1 e2

simpDef :: EDict -> Id -> Expr -> Expr -> Expr
simpDef d v (Val x) e2 = simp (define d v x) e2
simpDef d v e1 e2 = Def v e1 e2
