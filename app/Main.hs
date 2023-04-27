module Main where

-- To get the report do:

-- cabal run leqss-compare -- --output solver.html

-- cabal run leqss-compare -- --output solver_3x3.html
-- cabal run leqss-compare -- --output solver_10x10.html

import Criterion.Main
import BenchmarkData

import Solve_Vec
import Solve_matrix
import Solve_ll_state
import Solve_lu

import Solve_vu_state
import Solve_vu_unfold

import Solve_vu_unsafe
import Solve_rosetta1

main :: IO ()
main = do
  putStrLn "3*3"
  putStrLn $ "ll state"  <> (show $ solve_ll_state  ex1data_ll)
  putStrLn $ "vu state"  <> (show $ solve_vu_state  ex1data_vu)
  putStrLn $ "vu unfold" <> (show $ solve_vu_unfold ex1data_vu)

  putStrLn "10*10"
  putStrLn $ "ll state"  <> (show $ solve_ll_state  ex3data_ll)
  putStrLn $ "vu state"  <> (show $ solve_vu_state  ex3data_vu)
  putStrLn $ "vu unfold" <> (show $ solve_vu_unfold ex3data_vu)

