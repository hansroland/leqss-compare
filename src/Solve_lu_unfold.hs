{-# Language OverloadedStrings #-}

module Solve_lu_unfold (solve_lu_unfold) where

import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V

import Control.Monad.Trans.State.Strict

type Equation = V.Vector Double
type Matrix = [Equation]

cLIMIT :: Double
cLIMIT = 0.001

cINVALID :: T.Text
cINVALID = "Attempt to invert a non-invertible matrix"

solve_lu_unfold :: Matrix -> Either T.Text (V.Vector Double)
solve_lu_unfold mat = checkMatrix mat >>= calcTriangle >>= backInsert

checkMatrix :: Matrix -> Either T.Text Matrix
checkMatrix mat = checkMatrixSameLength mat >>= checkRowsCols

checkMatrixSameLength :: Matrix -> Either T.Text Matrix
checkMatrixSameLength mat = do
    let length1 = V.length $ head mat
    if all (\r -> V.length r == length1) mat
        then Right mat
        else Left " Not all equations have the same length"

checkRowsCols :: Matrix -> Either T.Text Matrix
checkRowsCols mat = do
    let numRows = length mat
        numCols = V.length $ head mat
    if numRows + 1 == numCols
        then Right mat
        else Left "Number of rows is not compatible with number of columns"

calcTriangle :: Matrix -> Either T.Text ([Equation], Matrix)
calcTriangle mat0 = runStateT (mapM (StateT . pivotStep) ops) mat0
  where
    ops = [2..(length mat0)]

    pivotStep :: Int -> Matrix -> Either T.Text (Equation, Matrix)
    pivotStep _ mat = do
       let (rowp, newmat) = getNextPivotRow mat
           pivot = V.head rowp
       if  abs pivot < cLIMIT
         then Left cINVALID
         else Right (rowp, fmap (newRow rowp) newmat)

    -- Find biggest pivot in first column.
    -- Remove row with pivot from matrix
    -- Return pivot and new matrix
    getNextPivotRow :: Matrix -> (Equation, Matrix)
    getNextPivotRow mat =
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

backInsert :: ([Equation], Matrix) -> Either T.Text (V.Vector Double)
backInsert (eqs , ress) = do
    let res = head ress
        piv = V.head res
        val = V.last res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cINVALID
      else Right $ V.fromList $ foldr stepInsert [xn] eqs

stepInsert :: Equation -> [Double] ->  [Double]
stepInsert equat xs =
   let piv = V.head equat
       as = (V.tail . V.init) equat
       s = V.last equat - V.sum (V.zipWith (*) as (V.fromList xs))
    in (s/piv) : xs
