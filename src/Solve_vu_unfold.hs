{-# Language OverloadedStrings #-}

module Solve_vu_unfold (solve_vu_unfold) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified VectorBuilder.Builder as B
import qualified VectorBuilder.Vector as C

import Debug.Trace
import BenchmarkData

type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cNONSOLVABLE:: T.Text
cNONSOLVABLE = "Attempt to solve a non-solvable equation system"

solve_vu_unfold :: Matrix -> Either T.Text (VU.Vector Double)
solve_vu_unfold mat = checkMatrix mat >>= calcTriangle >>= backInsert

-- -------------------------------------------------------------------
-- Check input
-- -------------------------------------------------------------------
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
    let numRows = V.length mat
        numCols = VU.length $ V.head mat
    if numRows + 1 == numCols
        then Right mat
        else Left "Number of rows is not compatible with number of columns"

-- -------------------------------------------------------------------
-- Calculation
-- -------------------------------------------------------------------
calcTriangle :: Matrix -> Either T.Text (V.Vector Equation)
calcTriangle mat0 = V.unfoldrExactNM (V.length mat0) pivotStep mat0
  where
    pivotStep :: Matrix -> Either T.Text (Equation, Matrix)
    pivotStep mat =
      if abs negPivot < cLIMIT
          then Left cNONSOLVABLE
          else Right (pivotrow, V.map newRow newMat)

     where
          ixprow = snd $ maximum $ V.imap (\ix e -> ((abs . VU.head) e, ix)) mat
          pivotrow = (V.!) mat ixprow
          -- newMat = V.ifilter (\ix _ -> ix /= ixprow) mat
          newMat = V.imapMaybe ixFilter mat   -- This is faster than V.ifilter !!
            where
            ixFilter :: Int -> a -> Maybe a
            ixFilter ix v
                | ix == ixprow = Nothing
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

backInsert :: V.Vector Equation -> Either T.Text (VU.Vector Double)
backInsert eqs = Right $ V.foldr stepInsert VU.empty eqs
  where
    stepInsert :: Equation -> VU.Vector Double -> VU.Vector Double
    stepInsert equat xs =
        let piv = VU.head equat
            as = (VU.tail . VU.init) equat
            s = VU.last equat - VU.sum (VU.zipWith (*) as xs)
        in cons (s/piv) xs                         -- TODO Check division !!!
    cons :: Double -> VU.Vector Double -> VU.Vector Double
    cons el vect = C.build builder
      where
        builder = B.singleton el <> B.vector vect

main :: IO ()
main = print $ solve_vu_unfold ex1data_vu

