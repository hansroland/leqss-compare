{-# Language OverloadedStrings #-}

module Solve_vu_unsafe (solve_vu_unsafe) where

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



solve_vu_unsafe :: Matrix -> Either T.Text (VU.Vector Double)
solve_vu_unsafe mat = calcTriangle mat >>= backInsert

calcTriangle :: Matrix -> Either T.Text (V.Vector Equation, Matrix)
calcTriangle mat = runStateT (mapM (StateT . pivotStep) ops) mat
    where
        ops = V.enumFromN 1 $ pred $ length mat

pivotStep :: Int -> Matrix -> Either T.Text (Equation, Matrix)
pivotStep _ mat0 = do
    let (rowp, mat) = getNextPivot mat0
        pivot = VU.unsafeHead rowp
    if  abs pivot < cLIMIT
      then Left cINVALID
      else Right $ (rowp, V.map (newRow rowp) mat)

-- Find biggest pivot in first column.
-- Remove row with pivot from matrix
-- Return pivot and new matrix
getNextPivot :: Matrix -> (Equation, Matrix)
getNextPivot mat =
    let len = V.length mat
        ixrow = snd $ maximum $ V.zip (V.map (abs . VU.unsafeHead) mat) (V.enumFromN 0 (len - 1))
        strtRows = V.unsafeSlice 0 ixrow mat
        rowp = V.unsafeIndex mat ixrow
        endRows = V.unsafeSlice (ixrow + 1) (len - ixrow -1) mat
    in  (rowp, strtRows <> endRows)

-- Apply the pivot to a row
newRow :: Equation -> Equation -> Equation
newRow rowp row = VU.unsafeTail $
                   VU.zipWith (+)
                     (applyPivot rowp row)
                     row

applyPivot :: Equation -> Equation -> Equation
applyPivot rowp row = VU.map (f *) rowp
   where
    f = negate $ VU.unsafeHead row / VU.unsafeHead rowp    -- Works only if pivot is in the first column

backInsert :: (V.Vector Equation, Matrix) -> Either T.Text (VU.Vector Double)
backInsert (eqs , ress) = do
    let res = V.unsafeHead ress
        piv = VU.unsafeHead res
        val = VU.unsafeLast res
        xn  = val / piv
    if  abs piv < cLIMIT
      then Left cINVALID
      else Right $ V.foldr stepInsert (VU.singleton xn) eqs

stepInsert :: Equation -> VU.Vector Double ->  VU.Vector Double
stepInsert equat xs =
   let piv = VU.unsafeHead equat
       as = (VU.unsafeTail . VU.unsafeInit) equat
       s = VU.unsafeLast equat - VU.sum (VU.zipWith (*) as xs)
    in VU.cons (s/piv)  xs
