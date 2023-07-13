module LeqssSpec (spec) where

import Test.Hspec

import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

import BenchmarkData
import Utils
import Solve_ll_state
import Solve_ll_unfold
import Solve_lu_state
import Solve_lu_unfold
import Solve_vu_state
import Solve_vu_unfold
import Solve_vu_state_unsafe
-- import Solve_new_vu

spec :: Spec
spec = do
  describe "Test with 3*3 equations" $ do
    it "List of lists state" $ do
        roundList <$> solve_ll_state ex1data_ll `shouldBe` result3x3
    it "List of lists unfold" $ do
        roundList <$> solve_ll_unfold ex1data_ll `shouldBe` result3x3

    it "List of unboxed vectors state" $ do
        roundVuToList <$>  solve_lu_state ex1data_lu `shouldBe` result3x3
    it "List of unboxed vectors unfold" $ do
        roundVuToList <$> solve_lu_unfold ex1data_lu `shouldBe` result3x3

    it "Vector of unboxed doubles state" $ do
        roundList . VU.toList <$> solve_vu_state ex1data_vu `shouldBe` result3x3
    it "Vector of unboxed doubles unfold" $ do
        roundList . VU.toList <$> solve_vu_unfold ex1data_vu `shouldBe` result3x3
    it "Vector of unboxed doubles state unsafe" $ do
        roundList . VU.toList <$> solve_vu_state_unsafe ex1data_vu `shouldBe` result3x3

  describe "Test with 10*10 equations" $ do
    it "List of lists state" $ do
        roundList <$> solve_ll_state ex3data_ll `shouldBe` result10x10
    it "Vector of unboxed doubles" $ do
        roundList . VU.toList <$> solve_vu_state ex3data_vu `shouldBe` result10x10
    it "Vector of unboxed doubles unfold" $ do
        roundList . VU.toList <$> solve_vu_unfold ex3data_vu `shouldBe` result10x10
    it "Vector of unboxed doubles state_unsafe" $ do
        roundList . VU.toList <$> solve_vu_state_unsafe ex3data_vu `shouldBe` result10x10

result3x3 :: Either T.Text [Double]
result3x3 = Right [2,3,-1]

result10x10 :: Either T.Text [Double]
result10x10 = Right [1,1,1,1,1,1,1,1,1,1]
