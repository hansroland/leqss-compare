module Main where

-- To get the report do:
-- cabal run leqss-compare -- --output solver_3x3.html
-- cabal run leqss-compare -- --output solver_10x10.html

import Criterion.Main
import BenchmarkData

import Solve_matrix
import Solve_ll
import Solve_lu
import Solve_vu
import Solve_vu_unsafe
import Solve_mu
import Solve_muf
import Solve_rosetta1

main :: IO ()
main = do
  putStrLn $ show ex1data_matrix
  putStrLn $ show ex1data_ll
  putStrLn $ show ex1data_lu
  putStrLn $ show ex1data_vu
  putStrLn $ show ex1rosettaMatrix
  putStrLn $ show ex1rosettaRight

  putStrLn $ show ex3data_matrix
  putStrLn $ show ex3data_ll
  putStrLn $ show ex3data_lu
  putStrLn $ show ex3data_vu
  putStrLn $ show ex3rosettaMatrix
  putStrLn $ show ex3rosettaRight

  defaultMain [
    bgroup "3*3"
                 [ --bench "matrix "   $ nf solve_matrix ex1data_matrix
                 -- , bench "rosetta code example1 "   $ nf (gauss ex1rosettaMatrix) ex1rosettaRight
                 bench "list of lists "   $ nf solve_ll ex1data_ll
                 , bench "list of unboxed vectors" $ nf solve_lu ex1data_lu
                 , bench "vector of unboxed vectors" $ nf solve_vu ex1data_vu
                 , bench "vector of unboxed vectors(unsafe)" $ nf solve_vu_unsafe ex1data_vu
                 , bench "mixed (list/vector) (state) of unboxed vectors" $ nf solve_mu ex1data_vu
                 , bench "mixed (list/vector) (fold) of unboxed vectors" $ nf solve_muf ex1data_vu
                 ]
    ]
{-


  defaultMain [
    bgroup "10*10"
                 [ -- bench "matrix "   $ nf solve_matrix ex3data_matrix
                 --, bench "rosetta code example1 "   $ nf (gauss ex3rosettaMatrix) ex3rosettaRight
                 bench "list of lists "   $ nf solve_ll ex3data_ll
                 , bench "list of unboxed vectors" $ nf solve_lu ex3data_lu
                 , bench "vector of unboxed vectors" $ nf solve_vu ex3data_vu
                 , bench "vector of unboxed vectors (unsafe)" $ nf solve_vu_unsafe ex3data_vu
                 , bench "mixed (list/vector) of unboxed vectors (state)" $ nf solve_mu ex3data_vu
                 , bench "mixed (list/vector) of unboxed vectors (fold)" $ nf solve_muf ex3data_vu
                 ]
    ]
-}
