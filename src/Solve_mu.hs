module Solve_mu (solve_mu) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Monad.Trans.State.Strict ( state, runState )

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

solve_mu :: Matrix -> [Double]
solve_mu mat = (backInsert . calcTriangle ) mat

-- Here [Equations] is a list. The sequence function seems to be much slower
-- for vectors than for lists.
calcTriangle :: Matrix -> ([Equation], Matrix)
calcTriangle mat = runState (sequence (state . pivotStep <$> ops)) mat
    where
        ops = [2..(length mat)]

pivotStep :: Int -> Matrix -> (Equation, Matrix)
pivotStep _ mat0 =
    let (rowp, mat) = getNextPivot mat0
        newmat = V.map (newRow rowp) mat
    in  (rowp, newmat)

-- Find biggest pivot in first column.
-- Remove row with pivot from matrix
-- Return pivot-row and new matrix
getNextPivot :: Matrix -> (Equation, Matrix)
getNextPivot mat =
    let len = V.length mat
        ixrow = snd $ maximum $ V.zip (V.map (abs . VU.head) mat) (V.enumFromN 0 (len - 1))
        strtRows = V.slice 0 ixrow mat
        rowp = (V.!) mat ixrow
        endRows = V.slice (ixrow + 1) (len - ixrow -1) mat
    in  (rowp, strtRows <> endRows)

-- Apply the pivot to a row
newRow :: Equation -> Equation -> Equation
newRow rowp row = VU.tail $
                   VU.zipWith (+)
                     (applyPivot rowp row)
                     row

applyPivot :: Equation -> Equation -> Equation
applyPivot rowp row = VU.map (f *) rowp
   where
    f = negate $ VU.head row / VU.head rowp    -- Works only if pivot is in the first column

backInsert :: ([Equation], Matrix) -> [Double]
backInsert (eqs , ress) =
    let res = V.head ress
        piv = VU.head res
        val = VU.last res
        xn  = val / piv
    in foldr stepInsert [xn] eqs

-- Here we are faster with lists than with unboxed vectors!
-- In the last line the cons operation for is o(1) for lists, but o(n) for vectors.
stepInsert :: Equation -> [Double] ->  [Double]
stepInsert equat xs =
   let piv = VU.head equat
       as = (VU.tail . VU.init) equat
       s = VU.last equat - (VU.sum  $ VU.zipWith (*) as (VU.fromList xs))
    in (s/piv) : xs
