import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set
import qualified Data.Map as Map

import Util
import Day2020_1

main :: IO ()
main = hspec $ do
  describe "util" $ do
    it "combinations" $ do
      combinations 1 [1] `shouldBe` [[1]]
      combinations 1 [1,2] `shouldBe` [[1], [2]]
      combinations 1 [1,2,3] `shouldBe` [[1], [2], [3]]

      combinations 2 [1] `shouldBe` []
      combinations 2 [1,2] `shouldBe` [[1,2]]
      combinations 2 [1,2,3] `shouldBe` [[1,2], [1,3], [2,3]]

      combinations 3 [1] `shouldBe` []
      combinations 3 [1,2] `shouldBe` []
      combinations 3 [1,2,3] `shouldBe` [[1,2,3]]
      combinations 3 [1,2,3,4] `shouldBe` [[1,2,3], [1,2,4], [1,3,4], [2,3,4]]

  describe "2020 day 1" $ do
    let example = [1721,979,366,299,675,1456]
    it "expenseReport" $ do
      expenseReport example `shouldBe` 514579

    it "expenseReport2" $ do
      expenseReport2 example `shouldBe` 241861950


