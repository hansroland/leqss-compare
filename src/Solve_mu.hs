{-# Language OverloadedStrings #-}

module Solve_mu (solve_mu) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Monad.Trans.State.Strict
    ( StateT(StateT, runStateT) )

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cINVALID :: T.Text
cINVALID = "Attempt to invert a non-invertible matrix"


solve_mu :: Matrix -> Either T.Text (VU.Vector Double)
solve_mu mat = checkMatrix mat >>= calcTriangle >>= backInsert

checkMatrix :: Matrix -> Either T.Text Matrix
checkMatrix mat = checkMatrixSameLength mat >>= checkRowsCols

checkMatrixSameLength :: Matrix -> Either T.Text Matrix
checkMatrixSameLength mat = do
    let length1 = VU.length $ V.head mat
    if all (\r -> (VU.length r == length1)) mat
        then Right mat
        else Left " Not all equations have the same length"

checkRowsCols :: Matrix -> Either T.Text Matrix
checkRowsCols mat = do
    let numRows = length mat
        numCols = VU.length $ V.head mat
    if numRows + 1 == numCols
        then Right mat
        else Left "Number of rows is not compatible with number of columns"


-- Here [Equations] is a list. The sequence function seems to be much slower
-- for vectors than for lists.
calcTriangle :: Matrix -> Either T.Text ([Equation], Matrix)
calcTriangle mat = runStateT (sequence (StateT . pivotStep <$> ops)) mat
    where
        ops = [2..(length mat)]

pivotStep :: Int -> Matrix -> Either T.Text (Equation, Matrix)
pivotStep _ mat0 = do
    let (rowp, mat) = getNextPivotRow mat0
        pivot = VU.head rowp
    if  abs pivot < cLIMIT
      then Left cINVALID
      else Right (rowp, V.map (newRow rowp) mat)

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
      else Right $ VU.fromList $ foldr stepInsert [xn] eqs

-- Here we are faster with lists than with unboxed vectors!
-- In the last line the cons operation for is o(1) for lists, but o(n) for vectors.
stepInsert :: Equation -> [Double] ->  [Double]
stepInsert equat xs =
   let piv = VU.head equat
       as = (VU.tail . VU.init) equat
       s = VU.last equat - (VU.sum  $ VU.zipWith (*) as (VU.fromList xs))
    in (s/piv) : xs
