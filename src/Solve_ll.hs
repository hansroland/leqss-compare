module Solve_ll (solve_ll) where

import Control.Monad.Trans.State.Strict

type Equation = [Double]
type Matrix = [Equation]

solve_ll :: Matrix -> [Double]
solve_ll mat = (backInsert . calcTriangle ) mat

calcTriangle :: Matrix -> ([Equation], Matrix)
calcTriangle mat = runState (sequence (state . pivotStep <$> ops)) mat
    where
        ops = [2..(length mat)]

pivotStep :: Int -> Matrix -> (Equation, Matrix)
pivotStep _ mat0 =
    let (rowp, mat) = getNextPivot mat0
        newmat = fmap (newRow rowp) mat
    in  (rowp, newmat)

-- Find biggest pivot in first column.
-- Remove row with pivot from matrix
-- Return pivot and new matrix
getNextPivot :: Matrix -> (Equation, Matrix)
getNextPivot mat =
    let ixrow = snd $ maximum $ zip (map (abs . head) mat) [0..]
        (heads, tails) = splitAt ixrow mat
        rowp = head tails
    in  (rowp, heads <> tail tails)

-- Apply the pivot to a row
newRow :: Equation -> Equation -> Equation
newRow rowp row = tail $
                   zipWith (+)
                     (applyPivot rowp row)
                     row

applyPivot :: Equation -> Equation -> Equation
applyPivot rowp row = fmap (f *) rowp
   where
    f = negate $ head row / head rowp    -- Works only if pivot is in the first column

backInsert :: ([Equation], Matrix) -> [Double]
backInsert (eqs , ress) =
    let res = head ress
        piv = head res
        val = last res
        xn  = val / piv
    in foldr stepInsert [xn] eqs

stepInsert :: Equation -> [Double] ->  [Double]
stepInsert equat xs =
   let piv = head equat
       as = (tail . init) equat
       s = last equat - (sum  $ zipWith (*) as xs)
    in (s/piv) : xs
