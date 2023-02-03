{-# Language OverloadedStrings #-}

module Solve_ll (solve_ll) where

import qualified Data.Text as T
import Control.Monad.Trans.State.Strict
    ( StateT(StateT, runStateT) )

type Equation = [Double]
type Matrix = [Equation]

cLIMIT :: Double
cLIMIT = 0.001

cINVALID :: T.Text
cINVALID = "Attempt to invert a non-invertible matrix"

solve_ll :: Matrix -> Either T.Text [Double]
solve_ll mat = checkMatrix mat >>= calcTriangle >>= backInsert

checkMatrix :: Matrix -> Either T.Text Matrix
checkMatrix mat = checkMatrixSameLength mat >>= checkRowsCols

checkMatrixSameLength :: Matrix -> Either T.Text Matrix
checkMatrixSameLength mat = do
    let length1 = length $ head mat
    if all (\r -> (length r == length1)) mat
        then Right mat
        else Left " Not all equations have the same length"

checkRowsCols :: Matrix -> Either T.Text Matrix
checkRowsCols mat = do
    let numRows = length mat
        numCols = length $ head mat
    if numRows + 1 == numCols
        then Right mat
        else Left "Number of rows is not compatible with number of columns"

calcTriangle :: Matrix -> Either T.Text ([Equation], Matrix)
calcTriangle mat = runStateT ((sequence (StateT . pivotStep <$> ops))) mat
    where
        ops = [2..(length mat)]

pivotStep :: Int -> Matrix -> Either T.Text (Equation, Matrix)
pivotStep _ mat0 = do
    let (rowp, mat) = getNextPivotRow mat0
        pivot = head rowp
    if  abs pivot < cLIMIT
      then Left cINVALID
      else Right (rowp, fmap (newRow rowp) mat)

-- Find biggest pivot in first column.
-- Remove row with pivot from matrix
-- Return pivot and new matrix
getNextPivotRow :: Matrix -> (Equation, Matrix)
getNextPivotRow mat =
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

backInsert :: ([Equation], Matrix) -> Either T.Text [Double]
backInsert (eqs , ress) = do
    let res = head ress
        piv = head res
        val = last res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cINVALID
      else Right $ foldr stepInsert [xn] eqs

stepInsert :: Equation -> [Double] ->  [Double]
stepInsert equat xs =
   let piv = head equat
       as = (tail . init) equat
       s = last equat - (sum  $ zipWith (*) as xs)
    in (s/piv) : xs
