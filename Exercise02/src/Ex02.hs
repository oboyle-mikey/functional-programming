{- oboylemi Michael O'Boyle -}
module Ex02 where

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype, honestly!
data T k d
  = A (T k d) (T k d) k d
  | B k d
  | C
  deriving (Eq, Show)

type IntFun = T Int Int -- binary tree with integer keys and data


-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> T k d -> T k d

-- Case 1: Insert into empty tree (C)
ins k d C = B k d

-- Case 2: Insert into one node tree (B)
ins k d (B k1 d1)
 |k < k1 = A (B k d) C k1 d1
 |k > k1 = A C (B k d) k1 d1
 |k == k1 = B k d

-- Case 3: Insert into any populated tree (A)

ins k d (A C (B k1 d1) ka da)
 |k < ka    = (A (B k d) (B k1 d1) ka da)
 |k > ka    = (A C (ins k d (B k1 d1)) ka da)
 |k == ka   = (A C (B k1 d1) k d)
 
ins k d (A (B k1 d1) C ka da)
 |k > ka    = (A (B k1 d1) (B k d) ka da)
 |k < ka    = (A (ins k d (B k1 d1)) C ka da)
 |k == ka   = (A (B k1 d1) C k d)
 
ins k d (A (B k1 d1) (B k2 d2) ka da)
 |k > ka    = (A (B k1 d1) (ins k d (B k2 d2)) ka da)
 |k < ka    = (A (ins k d (B k1 d1)) (B k2 d2) ka da)
 |k == ka   = (A (B k1 d1) (B k2 d2) k d)




-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => T k d -> k -> m d

lkp C k =

lkp (B k d) kl
 | kl == k   =
 | otherwise =

lkp (A (B k1 d1) (B k2 d2) ka da) kl
 | kl == k1 =
 | kl == k2 =
 | kl == ka =
 | otherwise




-- Part 3 : Tail-Recursive Statistics

{-
   It is possible to compute BOTH average and standard deviation
   in one pass along a list of data items by summing both the data
   and the square of the data.
-}
twobirdsonestone :: Double -> Double -> Int -> (Double, Double)
twobirdsonestone sum sumofsquares n
 = (average,sqrt variance)
 where
   nd = fromInteger $ toInteger n
   average = sum / nd
   variance = sumofsquares / nd - average * average

{-
  Implement the following function to take a list of numbers  (Double)
  and return a triple containing
   the sum of the numbers (Double)
   the sum of the squares of the numbers (Double)
   the length of the list (Int)
   You will need to figure out what extra arguments the function requires.
   Lokk at the tests for hints
-}
getsumsandlength :: Double -> Double -> Int -> [Double] -> (Double,Double,Int)
getsumsandlength d1 d2 i1 (x:xs) = getsumsandlength (d1+x) (d2+x*x) (i1+1) xs
getsumsandlength d1 d2 i1 []     = (d1,d2,i1)