module Utils where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- Rounding
factor :: Double
factor = 100000

-- round a double
roundDouble :: Double -> Double
roundDouble x = (fromInteger (round (factor * x))) / factor

-- round an unboxed vector
roundVectorU :: VU.Vector Double -> (VU.Vector Double)
roundVectorU = VU.map roundDouble

-- round an  vector
roundVectorV :: V.Vector Double -> (V.Vector Double)
roundVectorV = V.map roundDouble

roundList :: [Double] -> [Double]
roundList lst = roundDouble <$> lst

roundVuToList :: VU.Vector Double -> [Double]
roundVuToList = roundList . VU.toList
