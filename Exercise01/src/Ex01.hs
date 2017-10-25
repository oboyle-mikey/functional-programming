{- butrfeld Andrew Butterfield -}
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
eval [] (Val x) = Just x









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
    -- (2a) .... if there is mention of v in e2
    -- (2b) .... or any mention of v in e2 is inside another (Def v .. ..)

simpVar :: EDict -> Id -> Expr
simpVar d v = (Val 1e-99)

simpAdd :: EDict -> Expr -> Expr -> Expr
simpAdd d e1 e2 = (Val 1e-99)

simpSub :: EDict -> Expr -> Expr -> Expr
simpSub d e1 e2 = (Val 1e-99)

simpMul :: EDict -> Expr -> Expr -> Expr
simpMul d e1 e2 = (Val 1e-99)

simpDvd :: EDict -> Expr -> Expr -> Expr
simpDvd d e1 e2 = (Val 1e-99)

simpDef :: EDict -> Id -> Expr -> Expr -> Expr
simpDef d v e1 e2 = (Val 1e-99)
