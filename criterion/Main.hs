module Main where

-- To get the report do:

-- cabal run leqss-compare -- --output solver.html

-- cabal run leqss-compare -- --output solver_3x3.html
-- cabal run leqss-compare -- --output solver_10x10.html

import Criterion.Main
import BenchmarkData

import Solve_Vec
import Solve_matrix
import Solve_ll
import Solve_lu
import Solve_vu
import Solve_vu_state
import Solve_vu_unfold

import Solve_vu_unsafe
import Solve_rosetta1

main :: IO ()
main = do
  putStrLn $ show ex1data_matrix
  putStrLn $ show ex1data_ll
  putStrLn $ show ex1data_vu
  putStrLn $ show ex1rosettaMatrix
  putStrLn $ show ex1rosettaRight
  putStrLn $ show ex1dataVecMat
  putStrLn $ show ex1dataVecCoeffs

  putStrLn $ show ex3data_matrix
  putStrLn $ show ex3data_ll
  putStrLn $ show ex3data_vu
  putStrLn $ show ex3rosettaMatrix
  putStrLn $ show ex3rosettaRight

  defaultMain [
    bgroup "3*3"    [ bench "vector(3) list of lists-0" $ nf solve_ll ex1data_ll
                    , bench "vector(3) list of lists-1" $ nf solve_ll ex1data_ll
                    , bench "vector (3) unboxed vectors state" $ nf solve_vu_state ex1data_vu
                    , bench "vector (3) unboxed vectors unfold" $ nf solve_vu_unfold ex1data_vu
                    , bench "Hackage Vec(3)" $ nf (solve_Vec ex1dataVecMat) ex1dataVecCoeffs
                    ],
    bgroup "10x10"  [ bench "vector(10 list of lists-0" $ nf solve_ll ex3data_ll
                    , bench "vector(10 list of lists-1" $ nf solve_ll ex3data_ll
                    , bench "vector (10) of unboxed vectors state" $ nf solve_vu_state ex3data_vu
                    , bench "vector (10) of unboxed vectors unfold" $ nf solve_vu_unfold ex3data_vu
                    -- , bench "vector Hackage Vec (10)" $ nf (solve_Vec ex3dataVecMat) ex3dataVecCoeffs
                    ]
                    ]


{-
  defaultMain [
    bgroup "3*3"
                 [ bench "Hackage matrix "   $ nf solve_matrix ex1data_matrix
                 , bench "Rosetta code example1 "   $ nf (gauss ex1rosettaMatrix) ex1rosettaRight
                 , bench "Hackage Vec "   $ nf (solve_Vec ex1dataVecMat) ex1dataVecCoeffs
                 , bench "list of lists "   $ nf solve_ll ex1data_ll
                 , bench "list of unboxed vectors" $ nf solve_lu ex1data_lu
                 , bench "vector of unboxed vectors NEW" $ nf solve_new_vu ex1data_vu
                 , bench "vector of unboxed vectors" $ nf solve_vu ex1data_vu
                 , bench "vector of unboxed vectors(unsafe)" $ nf solve_vu_unsafe ex1data_vu
                 ]
    ]


  defaultMain [
    bgroup "10*10"
                 [ bench "Hackage matrix "   $ nf solve_matrix ex3data_matrix
                 , bench "rosetta code example1 "   $ nf (gauss ex3rosettaMatrix) ex3rosettaRight
                 , bench "list of lists "   $ nf solve_ll ex3data_ll
                 , bench "list of unboxed vectors" $ nf solve_lu ex3data_lu
                 , bench "vector of unboxed vectors" $ nf solve_vu ex3data_vu
                 , bench "vector of unboxed vectors NEW" $ nf solve_new_vu ex3data_vu
                 , bench "vector of unboxed vectors (unsafe)" $ nf solve_vu_unsafe ex3data_vu
                 ]
    ]
-}
