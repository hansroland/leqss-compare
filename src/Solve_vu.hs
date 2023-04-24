{-# Language OverloadedStrings #-}

module Solve_vu (solve_vu) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Monad.Trans.State.Strict

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cINVALID :: T.Text
cINVALID = "Attempt to invert a non-invertible matrix"

solve_vu :: Matrix -> Either T.Text (VU.Vector Double)
solve_vu mat = checkMatrix mat >>= calcTriangle >>= backInsert

-- Run different sanity checks on the input data
checkMatrix :: Matrix -> Either T.Text Matrix
checkMatrix mat = checkMatrixSameLength mat >>= checkRowsCols

-- Check: All rows have the same length
checkMatrixSameLength :: Matrix -> Either T.Text Matrix
checkMatrixSameLength mat = do
    let length1 = VU.length $ V.head mat
    if all (\r -> VU.length r == length1) (V.tail mat)
        then Right mat
        else Left "Not all equations have the same length"

-- Check: The number of cols is one bigger than the number of rows
checkRowsCols :: Matrix -> Either T.Text Matrix
checkRowsCols mat = do
    let numRows = length mat
        numCols = VU.length $ V.head mat
    if numRows + 1 == numCols
        then Right mat
        else Left "Number of rows is not compatible with number of columns"

--
calcTriangle :: Matrix -> Either T.Text (V.Vector Equation, Matrix)
calcTriangle mat = runStateT (mapM (StateT . pivotStep) ops) mat
    where
        ops = V.replicate (length mat - 1) ()

pivotStep :: () -> Matrix -> Either T.Text (Equation, Matrix)
pivotStep _ mat0 = do
    let (rowp, mat) = getNextPivot
        pivot = VU.head rowp
    if  abs pivot < cLIMIT
      then Left cINVALID
      else Right (rowp, V.map (newRow rowp) mat)
  where
    -- Find biggest pivot in first column.
    -- Remove row with pivot from matrix
    -- Return pivot and new matrix
    getNextPivot :: (Equation, Matrix)
    getNextPivot =
        let len = V.length mat0
            ixrow = snd $ maximum $ V.zip (V.map (abs . VU.head) mat0) (V.enumFromN 0 (len - 1))
            rowp = (V.!) mat0 ixrow
            newmat = V.ifilter (\i _ -> i /= ixrow) mat0
        in  (rowp, newmat)

-- Apply the pivot to a row
newRow :: Equation -> Equation -> Equation
newRow rowp row = VU.zipWith (+) (VU.tail applyPivot) (VU.tail row)
  where
    applyPivot = VU.map (f *) rowp
    f = negate $ VU.head row / VU.head rowp    -- Works only if pivot is in the first column

backInsert :: (V.Vector Equation, Matrix) -> Either T.Text (VU.Vector Double)
backInsert (eqs, ress) = do
    let res = V.head ress
        piv = VU.head res
        val = VU.last res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cINVALID
      else Right $ V.foldr stepInsert (VU.singleton xn) eqs

stepInsert :: Equation -> VU.Vector Double ->  VU.Vector Double
stepInsert equat xs =
   let piv = VU.head equat
       as = VU.unsafeSlice 1 (VU.length equat - 2) equat
       s = VU.last equat - VU.sum  (VU.zipWith (*) as xs)
    in VU.cons (s/piv)  xs
