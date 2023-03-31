module BenchmarkData where

import Data.Matrix
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import qualified Data.Vec.Base as Vec
import qualified Data.Vec.LinAlg as Vec

-- -------------------------------------------------------------- Example 1
-- Example from
-- https://en.wikipedia.org/wiki/Gaussian_elimination

ex1row1,ex1row2,ex1row3 :: [Double]
ex1row1 = [2, 1, -1, 8]
ex1row2 = [-3, -1, 2, -11]
ex1row3 = [-2, 1, 2,  -3]

ex1rows :: [[Double]]
ex1rows = [ex1row1, ex1row2, ex1row3]

ex1rosettaMatrix :: [[Double]]
ex1rosettaMatrix = init <$> ex1rows

ex1rosettaRight :: [[Double]]
ex1rosettaRight = pure . last <$> ex1rows


ex1data_ll :: [[Double]]
ex1data_ll = ex1rows


ex1data_vu :: V.Vector (VU.Vector Double)
ex1data_vu = V.fromList $ VU.fromList <$> ex1data_ll

ex1data_matrix :: Matrix Double
ex1data_matrix = fromLists ex1rows

ex1dataVecMat :: Vec.Mat33 Double
ex1dataVecMat = Vec.matFromLists $ map init ex1rows

ex1dataVecCoeffs :: Vec.Vec3 Double
ex1dataVecCoeffs = Vec.fromList $ map last ex1rows
-- -------------------------------------------------------------- Example 3
-- Example with a 10 * 10 matrix

ex3row1, ex3row2, ex3row3, ex3row4, ex3row5, ex3row6, ex3row7, ex3row8, ex3row9, ex3row10 :: [Double]
ex3row1 = [ 42, -44,  21, -10,  44, -55, -25,  33,  56, -13,  49]
ex3row2 = [-24,  17,  13, -49,  36,  -8,  42, -27,  16, -53, -37]
ex3row3 = [ 14, -27,  34, -52, -11,  48, -29,  29,  -3,  43,  46]
ex3row4 = [-23, -36,  45,  61,   1,  22, -17,  23, -43, -54, -21]
ex3row5 = [  5,   7, -19,  63,  11, -32,   4, -66,   9,  51,  33]
ex3row6 = [-72,  -8,  20, -30,  -2,  -6, -14,  54,  67, -43, -34]
ex3row7 = [-37, -81,  26, -16, -32,  76,  74,  99,  32, -65,  76]
ex3row8 = [  8,  18, -46,  82, -14, -22,  37, -39, -17,  12,  19]
ex3row9 = [ 14, -54,  37,  27, -67,  37, -71,  34, -66,  77, -32]
ex3row10= [ 94, -93,  92,  12, -13, -14,  92, -43,  18, -53,  92]


subLast :: [Double] -> Double -> Double
subLast  ss s = s - (last ss)

check :: [Double] -> Double
check xs = subLast xs $ sum (init xs)

ex3rows :: [[Double]]
ex3rows = [ex3row1, ex3row2, ex3row3, ex3row4, ex3row5, ex3row6, ex3row7, ex3row8, ex3row9, ex3row10]

ex3rosettaMatrix :: [[Double]]
ex3rosettaMatrix = init <$> ex3rows

ex3rosettaRight :: [[Double]]
ex3rosettaRight = pure . last <$> ex3rows


ex3data_ll :: [[Double]]
ex3data_ll = ex3rows

ex3data_lu :: [VU.Vector Double]
ex3data_lu = VU.fromList <$> ex3data_ll

ex3data_vu :: V.Vector (VU.Vector Double)
ex3data_vu = V.fromList $ VU.fromList <$> ex3data_ll

ex3data_matrix :: Matrix Double
ex3data_matrix = fromLists ex3rows
