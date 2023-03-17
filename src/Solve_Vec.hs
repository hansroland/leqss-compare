module Solve_Vec (solve_Vec) where

import Data.Vec.Base
import Data.Vec.LinAlg

import BenchmarkData

solve_Vec :: Mat33 Double -> Vec3 Double->  Either String [Double]
solve_Vec mat coeffs =
    let mbSolution = solve mat coeffs
    in  case mbSolution of
          Just vect -> Right $ toList vect
          Nothing  -> Left "Equation system has no solution"