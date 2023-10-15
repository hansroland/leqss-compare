{-# Language OverloadedStrings #-}

module Solve_vu_state_unsafe (solve_vu_state_unsafe) where

import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified VectorBuilder.Builder as B
import qualified VectorBuilder.Vector as C
import Control.Monad.Trans.State.Strict
    ( StateT(StateT, runStateT) )


type Equation = VU.Vector Double
type Matrix = V.Vector Equation

cLIMIT :: Double
cLIMIT = 0.001

cNONSOLVABLE:: T.Text
cNONSOLVABLE = "Attempt to solve a non-solvable equation system"

solve_vu_state_unsafe :: Matrix -> Either T.Text (VU.Vector Double)
solve_vu_state_unsafe mat = checkMatrix mat >>= calcTriangle >>= backInsert

checkMatrix :: Matrix -> Either T.Text Matrix
checkMatrix mat = checkMatrixSameLength mat >>= checkRowsCols

checkMatrixSameLength :: Matrix -> Either T.Text Matrix
checkMatrixSameLength mat = do
    let length1 = VU.length $ V.unsafeHead mat
    if all (\r -> VU.length r == length1) mat
        then Right mat
        else Left "Not all equations have the same length"

checkRowsCols :: Matrix -> Either T.Text Matrix
checkRowsCols mat = do
    let numRows = length mat
        numCols = VU.length $ V.unsafeHead mat
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
        else Right (pivotrow, V.map newRow newMat)
      where
        ixprow = V.maxIndexBy (\x y -> compare (abshead x) (abshead y)) mat
        abshead = abs . VU.unsafeHead
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
                     (applyPivot (VU.unsafeHead row))
                     (VU.unsafeTail row)

        applyPivot :: Double -> Equation
        applyPivot hdRow = VU.map (hdRow / negPivot *) unsafeTailprow
        -- The next 2 values do not change between rows in applyPivot!
        unsafeTailprow = VU.unsafeTail pivotrow
        negPivot = negate $ VU.unsafeHead pivotrow

backInsert :: (V.Vector Equation, Matrix) -> Either T.Text (VU.Vector Double)
backInsert (eqs , ress) = do
    let res = V.unsafeHead ress
        piv = VU.unsafeHead res
        val = VU.unsafeLast res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cNONSOLVABLE
      else Right $ V.foldr stepInsert (VU.singleton xn) eqs
  where
    stepInsert :: Equation -> VU.Vector Double ->  VU.Vector Double
    stepInsert equat xs =
        let piv = VU.unsafeHead equat
            as = (VU.unsafeTail . VU.init) equat
            s = VU.unsafeLast equat - VU.sum (VU.zipWith (*) as xs)
        in cons (s/piv) xs
    cons :: Double -> VU.Vector Double -> VU.Vector Double
    cons el vect = C.build builder
      where
        builder = B.singleton el <> B.vector vect
