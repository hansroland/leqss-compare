-- Solve a linear equation system with the matrix package from Hackage
module Solve_matrix where

import Utils

import Data.Matrix
import qualified Data.Vector as V
-- import Control.DeepSeq ( force )

-- Solve the matrix
solve_matrix :: Matrix Double -> Either String (V.Vector Double)
solve_matrix mat =
    let echelon = rref mat
        lastCol = ncols <$> echelon
        -- round the last column
    in  roundVectorV <$> (getCol <$> lastCol <*> echelon)




