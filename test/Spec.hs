import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set
import qualified Data.Map as Map

import Day1

main :: IO ()
main = hspec $ do
  describe "day1" $ do
    let example = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
    it "countIncreases" $ do
      countIncreases [] `shouldBe` 0
      countIncreases example `shouldBe` 7

    it "slidingIncreases" $ do
      slidingIncreases [1, 2] `shouldBe` 0
      slidingIncreases example `shouldBe` 5

