

module Solve_lu (solve_lu) where

import qualified Data.Vector.Unboxed as V

import Control.Monad.Trans.State.Strict

type Equation = V.Vector Double
type Matrix = [Equation]

-- Example from
-- https://en.wikipedia.org/wiki/Gaussian_elimination


solve_lu :: Matrix -> [Double]
solve_lu mat = (backInsert . calcTriangle ) mat

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
    let ixrow = snd $ maximum $ zip (map (abs . V.head) mat) [0..]
        (heads, tails) = splitAt ixrow mat
        rowp = head tails
    in  (rowp, heads <> tail tails)

-- Apply the pivot to a row
newRow :: Equation -> Equation -> Equation
newRow rowp row = V.tail $
                   V.zipWith (+)
                     (applyPivot rowp row)
                     row

applyPivot :: Equation -> Equation -> Equation
applyPivot rowp row = V.map (f *) rowp
   where
    f = negate $ V.head row / V.head rowp    -- Works only if pivot is in the first column

backInsert :: ([Equation], Matrix) -> [Double]
backInsert (eqs , ress) =
    let res = head ress
        piv = V.head res
        val = V.last res
        xn  = val / piv
    in foldr stepInsert [xn] eqs

stepInsert :: Equation -> [Double] ->  [Double]
stepInsert equat xs =
   let piv = V.head equat
       as = (V.tail . V.init) equat
       s = V.last equat - (V.sum  $ V.zipWith (*) as (V.fromList xs))
    in (s/piv) : xs
