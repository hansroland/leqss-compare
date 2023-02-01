module Solve_vu (solve_vu) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Monad.Trans.State.Strict

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

solve_vu :: Matrix -> VU.Vector Double
solve_vu mat = (backInsert . calcTriangle ) mat

calcTriangle :: Matrix -> (V.Vector Equation, Matrix)
calcTriangle mat = runState (sequence (state . pivotStep <$> ops)) mat
    where
        ops = V.enumFromN 1 $ pred $ length mat

pivotStep :: Int -> Matrix -> (Equation, Matrix)
pivotStep _ mat0 =
    let (rowp, mat) = getNextPivot mat0
        newmat = V.map (newRow rowp) mat
    in  (rowp, newmat)

-- Find biggest pivot in first column.
-- Remove row with pivot from matrix
-- Return pivot and new matrix
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

backInsert :: (V.Vector Equation, Matrix) -> VU.Vector Double
backInsert (eqs , ress) =
    let res = V.head ress
        piv = VU.head res
        val = VU.last res
        xn  = val / piv
    in V.foldr stepInsert (VU.singleton xn) eqs

stepInsert :: Equation -> VU.Vector Double ->  VU.Vector Double
stepInsert equat xs =
   let piv = VU.head equat
       as = (VU.tail . VU.init) equat
       s = VU.last equat - (VU.sum  $ VU.zipWith (*) as (xs))
    in VU.cons (s/piv)  xs
