{-# Language OverloadedStrings #-}

module Solve_new_vu (solve_new_vu) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Monad.Trans.State.Strict

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cNONSOLVABLE:: T.Text
cNONSOLVABLE = "Attempt to solve a non-solvable equation system"

solve_new_vu :: Matrix -> Either T.Text (VU.Vector Double)
solve_new_vu mat = checkMatrix mat >>= calcTriangle >>= backInsert

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


calcTriangle :: Matrix -> Either T.Text (V.Vector Equation, Matrix)
calcTriangle mat = runStateT (sequence (StateT . pivotStep <$> ops)) mat
    where
        ops = V.enumFromN 1 $ pred $ length mat

pivotStep :: Int -> Matrix -> Either T.Text (Equation, Matrix)
pivotStep _ mat0 = do
    if  abs pivot < cLIMIT
      then Left cNONSOLVABLE
      else Right $ (rowp, V.map newRow mat)
  where
    (rowp, mat) = getNextPivot mat0
    tailrowp = VU.tail rowp
    pivot = VU.head rowp
    -- fact = negate $ 1 / pivot
    -- Apply the pivot to a row
    newRow :: Equation -> Equation
    newRow row = VU.zipWith (+)
                     (applyPivot (VU.head row))
                     (VU.tail row)

    applyPivot :: Double -> Equation
    applyPivot hdRow = VU.map (f *) tailrowp
      where
       f = negate $ hdRow / pivot    -- Works only if pivot is in the first column


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


backInsert :: (V.Vector Equation, Matrix) -> Either T.Text (VU.Vector Double)
backInsert (eqs , ress) = do
    let res = V.head ress
        piv = VU.head res
        val = VU.last res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cNONSOLVABLE
      else Right $ V.foldr stepInsert (VU.singleton xn) eqs

stepInsert :: Equation -> VU.Vector Double ->  VU.Vector Double
stepInsert equat xs =
   let piv = VU.head equat
       as = (VU.tail . VU.init) equat
       s = VU.last equat - (VU.sum  $ VU.zipWith (*) as (xs))
    in VU.cons (s/piv)  xs
