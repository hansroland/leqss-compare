{-# Language OverloadedStrings #-}

module Solve_muf (solve_muf) where

import Data.Foldable ( foldlM )
import Data.List ( foldl' )
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- import Debug.Trace

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cINVALID :: T.Text
cINVALID = "Attempt to invert a non-invertible matrix"

solve_muf :: Matrix -> Either T.Text (VU.Vector Double)
solve_muf mat = calcTriangle mat >>= backInsert

-- Here [Equations] is a list. The sequence function seems to be much slower
-- for vectors than for lists.
calcTriangle :: Matrix -> Either T.Text ([Equation], Matrix)
calcTriangle mat = foldlM pivotStep ([], mat) [2..(length mat)]

pivotStep :: ([Equation],Matrix) -> Int -> Either T.Text ([Equation], Matrix)
pivotStep (eqs, mat0) _ = do
    let (rowp, mat) = getNextPivotRow mat0
        pivot = VU.head rowp
    if  abs pivot < cLIMIT
      then Left cINVALID
      else Right (rowp : eqs, V.map (newRow rowp) mat)

-- Find biggest pivot in first column.
-- Remove row with pivot from matrix
-- Return pivot-row and new matrix
getNextPivotRow :: Matrix -> (Equation, Matrix)
getNextPivotRow mat =
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

backInsert :: ([Equation], Matrix) -> Either T.Text (VU.Vector Double)
backInsert (eqs , ress) = do
    let res = V.head ress
        piv = VU.head res
        val = VU.last res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cINVALID
      else Right $ VU.fromList $ foldl' stepInsert [xn] eqs

-- Here we are faster with lists than with unboxed vectors!
-- In the last line the cons operation for is o(1) for lists, but o(n) for vectors.
stepInsert :: [Double] -> Equation -> [Double]
stepInsert xs equat =
   let piv = VU.head equat
       as = (VU.tail . VU.init) equat
       s = VU.last equat - (VU.sum  $ VU.zipWith (*) as (VU.fromList xs))
    in (s/piv) : xs