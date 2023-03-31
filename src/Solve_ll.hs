{-# Language OverloadedStrings #-}

module Solve_ll (solve_ll) where

import qualified Data.Text as T
import Control.Monad.Trans.State.Strict

import BenchmarkData

type Equation = [Double]
type Matrix = [Equation]

cLIMIT :: Double
cLIMIT = 0.001

cNONSOLVABLE:: T.Text
cNONSOLVABLE = "Attempt to solve a non-solvable equation system"

solve_ll :: Matrix -> Either T.Text [Double]
solve_ll mat = checkMatrix mat >>= calcTriangle >>= backInsert

checkMatrix :: Matrix -> Either T.Text Matrix
checkMatrix mat = checkMatrixSameLength mat >>= checkRowsCols

checkMatrixSameLength :: Matrix -> Either T.Text Matrix
checkMatrixSameLength mat = do
    let length1 = length $ head mat
    if all (\r -> length r == length1) mat
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
calcTriangle mat0 = runStateT (mapM (StateT . pivotStep)  ops) mat0
  where
    len0 = length mat0
    ops = replicate (pred len0) 0
    venum = [(0::Int)..len0]

    pivotStep :: Int -> Matrix -> Either T.Text (Equation, Matrix)
    pivotStep _ mat =
        if abs negPivot < cLIMIT
        then Left cNONSOLVABLE
        else Right (pivotrow, map newRow newMat)
      where
        ixprow = snd $ maximum $ zip (map (abs . head) mat) venum
        pivotrow = mat !! ixprow
        newMat = take ixprow mat <> drop (ixprow + 1) mat

        -- Apply the pivot to a row
        newRow :: Equation -> Equation
        newRow row = zipWith (+)
                     (applyPivot (head row))
                     (tail row)

        applyPivot :: Double -> Equation
        applyPivot hdRow = map (hdRow / negPivot *) tailprow
        -- The next 2 values do not change between rows in applyPivot!
        tailprow = tail pivotrow
        negPivot = negate $ head pivotrow

backInsert :: ( [Equation], Matrix) -> Either T.Text [Double]
backInsert (eqs , ress) = do
    let res = head ress
        piv = head res
        val = last res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cNONSOLVABLE
      else Right $ foldr stepInsert [xn] eqs

stepInsert :: Equation -> [Double] -> [Double]
stepInsert equat xs =
   let piv = head equat
       as = (tail . init) equat
       s = last equat - sum (zipWith (*) as xs)
    in (s/piv) : xs

main :: IO ()
main = print $ solve_ll ex1data_ll