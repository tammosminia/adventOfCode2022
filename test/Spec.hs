import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set
import qualified Data.Map as Map

import Util
import Day2020_1
import Day2020_2
import Day1

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

    it "count" $ do
      count (>2) [1,2,3,4,5] `shouldBe` 3
      
  describe "2020 day 1" $ do
    let example = [1721,979,366,299,675,1456]
    it "expenseReport" $ do
      expenseReport example `shouldBe` 514579

    it "expenseReport2" $ do
      expenseReport2 example `shouldBe` 241861950

  describe "2020 day 2" $ do
    it "validPassword" $ do
      validPassword "1-3 a: abcde" `shouldBe` True
      validPassword "1-3 b: cdefg" `shouldBe` False
      validPassword "2-9 c: ccccccccc" `shouldBe` True

    it "validPassword2" $ do
      validPassword2 "1-3 a: abcde" `shouldBe` True
      validPassword2 "1-3 a: cbade" `shouldBe` True
      validPassword2 "1-3 a: abade" `shouldBe` False
      validPassword2 "1-3 b: cdefg" `shouldBe` False
      validPassword2 "2-9 c: aaaaaaaaa" `shouldBe` False
      validPassword2 "2-9 c: acaaaaaaa" `shouldBe` True
      validPassword2 "2-9 c: aaaaaaaac" `shouldBe` True
      validPassword2 "2-9 c: ccccccccc" `shouldBe` False

  describe "day 1" $ do
    let example = [[1000,2000,3000],[4000],[5000,6000],[7000,8000,9000],[10000]]
    it "day1a" $ do
      day1a example `shouldBe` 24000
    it "day1b" $ do
      day1b example `shouldBe` 45000

