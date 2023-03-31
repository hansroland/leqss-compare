{-# Language OverloadedStrings #-}

module Solve_vu_state (solve_vu_state) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Control.Monad.Trans.State.Strict
    ( StateT(StateT, runStateT) )

import BenchmarkData

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cNONSOLVABLE:: T.Text
cNONSOLVABLE = "Attempt to solve a non-solvable equation system"

solve_vu_state :: Matrix -> Either T.Text (VU.Vector Double)
solve_vu_state mat = checkMatrix mat >>= calcTriangle >>= backInsert

checkMatrix :: Matrix -> Either T.Text Matrix
checkMatrix mat = checkMatrixSameLength mat >>= checkRowsCols

checkMatrixSameLength :: Matrix -> Either T.Text Matrix
checkMatrixSameLength mat = do
    let length1 = VU.length $ V.head mat
    if all (\r -> VU.length r == length1) mat
        then Right mat
        else Left "Not all equations have the same length"

checkRowsCols :: Matrix -> Either T.Text Matrix
checkRowsCols mat = do
    let numRows = length mat
        numCols = VU.length $ V.head mat
    if numRows + 1 == numCols
        then Right mat
        else Left "Number of rows is not compatible with number of columns"

-- -------------------------------------------------------------------
-- Calculation
-- -------------------------------------------------------------------
calcTriangle :: Matrix -> Either T.Text (V.Vector Equation, Matrix)
calcTriangle mat0 = runStateT (mapM (StateT . pivotStep) ops) mat0
  where
    len0 = V.length mat0
    ops = V.replicate (pred len0) 0

    pivotStep :: Int -> Matrix -> Either T.Text (Equation, Matrix)
    pivotStep _ mat =
        if abs negPivot < cLIMIT
        then Left cNONSOLVABLE
        else Right $ (pivotrow, V.map newRow newMat)
      where
        ixprow = snd $ maximum $ V.imap (\ix e -> ((abs . VU.head) e, ix)) mat
        pivotrow = (V.!) mat ixprow
        -- newMat = V.ifilter (\ix _ -> ix /= ixprow) mat
        newMat = V.imapMaybe ixFilter mat   -- This is faster than V.ifilter !!
          where
            ixFilter :: Int -> a -> Maybe a
            ixFilter ix v
                | ix == ixprow  = Nothing
                | otherwise  = Just v

        -- Apply the pivot to a row
        newRow :: Equation -> Equation
        newRow row = VU.zipWith (+)
                     (applyPivot (VU.head row))
                     (VU.tail row)

        applyPivot :: Double -> Equation
        applyPivot hdRow = VU.map (hdRow / negPivot *) tailprow
        -- The next 2 values do not change between rows in applyPivot!
        tailprow = VU.tail pivotrow
        negPivot = negate $ VU.head pivotrow

backInsert :: (V.Vector Equation, Matrix) -> Either T.Text (VU.Vector Double)
backInsert (eqs , ress) = do
    let res = V.head ress
        piv = VU.head res
        val = VU.last res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cNONSOLVABLE
      else Right $ V.foldr stepInsert (VU.singleton xn) eqs
  where
    stepInsert :: Equation -> VU.Vector Double ->  VU.Vector Double
    stepInsert equat xs =
        let piv = VU.head equat
            as = (VU.tail . VU.init) equat
            s = VU.last equat - VU.sum (VU.zipWith (*) as xs)
        in VU.cons (s/piv) xs

main :: IO ()
main = print $ solve_vu_state ex1data_vu


