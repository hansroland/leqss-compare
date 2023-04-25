module LeqssSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

import BenchmarkData
import Utils
import Solve_ll
import Solve_vu_state
import Solve_vu_unfold
import Solve_vu_unsafe
-- import Solve_new_vu

spec :: Spec
spec = do
  describe "Test with 3*3 equations" $ do
    it "List of Lists" $ do
        roundList <$> solve_ll ex1data_ll `shouldBe` result3x3
    it "Vector of unboxed doubles state" $ do
        roundList . VU.toList <$> solve_vu_state ex1data_vu `shouldBe` result3x3
    it "Vector of unboxed doubles unfold" $ do
        roundList . VU.toList <$> solve_vu_unfold ex1data_vu `shouldBe` result3x3
    it "Vector of unboxed doubles unsafe" $ do
        roundList . VU.toList <$> solve_vu_unsafe ex1data_vu `shouldBe` result3x3

  describe "Test with 10*10 equations" $ do
    it "List of Lists" $ do
        roundList <$> solve_ll ex3data_ll `shouldBe` result10x10
    it "Vector of unboxed doubles" $ do
        roundList . VU.toList <$> solve_vu_state ex3data_vu `shouldBe` result10x10
    it "Vector of unboxed doubles unfold" $ do
        roundList . VU.toList <$> solve_vu_unfold ex3data_vu `shouldBe` result10x10
    it "Vector of unboxed doubles unsafe" $ do
        roundList . VU.toList <$> solve_vu_unsafe ex3data_vu `shouldBe` result10x10

result3x3 :: Either T.Text [Double]
result3x3 = Right [2,3,-1]

result10x10 :: Either T.Text [Double]
result10x10 = Right [1,1,1,1,1,1,1,1,1,1]
