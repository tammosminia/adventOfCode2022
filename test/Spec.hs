import Test.Hspec
import Test.QuickCheck

import qualified Data.Set as Set
import qualified Data.Map as Map

import Util
import qualified Grid
import Day2020_1
import Day2020_2
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8

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
      
    it "takeUntil" $ do
      takeUntil (>2) [1,2,3,4,5] `shouldBe` [1,2,3]
      
  describe "grid" $ do
    it "allPoints" $ do
      Grid.allPoints (Grid.init ["12", "34"])  `shouldBe` [(0,0), (0,1), (1,0), (1,1)]
      
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

  describe "day 2" $ do
    let example = ["A Y","B X","C Z"]
    it "day2a" $ do
      day2a example `shouldBe` 15
    it "day2b" $ do
      day2b example `shouldBe` 12

  describe "day 3" $ do
    let example = ["vJrwpWtwJgWrhcsFMMfFFhFp","jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL","PmmdzqPrVvPwwTWBwg","wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn","ttgJtRGJQctTZtZT","CrZsJsPPZsGzwwsLwLmpwMDw"]
    it "day3a" $ do
      day3a example `shouldBe` 157

    it "itemPriority" $ do
      itemPriority 'z' `shouldBe` 26
      itemPriority 'Z' `shouldBe` 52
      
    it "day3b" $ do
      day3b example `shouldBe` 70

  describe "day 4" $ do
    let example = [[2,4,6,8],[2,3,4,5],[5,7,7,9],[2,8,3,7],[6,6,4,6],[2,6,4,8]]
    it "day4a" $ do
      day4a example `shouldBe` 2

    it "day4b" $ do
      day4b example `shouldBe` 4

  describe "day 5" $ do
    let exampleStacks = ["NZ", "DCM", "P"]
    let exampleMoves = ["move 1 from 2 to 1","move 3 from 1 to 3","move 2 from 2 to 1","move 1 from 1 to 2"]
    it "day5a" $ do
      day5a exampleStacks exampleMoves `shouldBe` "CMZ"
      
    it "day5b" $ do
      day5b exampleStacks exampleMoves `shouldBe` "MCD"

  describe "day 6" $ do
    it "day6a" $ do
      day6a "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 7
      day6a "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` 5
      day6a "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` 6
      day6a "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` 10
      day6a "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` 11

    it "day6b" $ do
      day6b "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 19

  describe "day 7" $ do
    let example = ["$ cd /","$ ls","dir a","14848514 b.txt","8504156 c.dat","dir d","$ cd a","$ ls","dir e","29116 f","2557 g","62596 h.lst","$ cd e","$ ls","584 i","$ cd ..","$ cd ..","$ cd d","$ ls","4060174 j","8033020 d.log","5626152 d.ext","7214296 k"]
    it "parseInput" $ do
      print $ show $ parseInput ["$ cd /","$ ls","dir a"]
      print $ show $ parseInput example

    it "day7a" $ do
      day7a example `shouldBe` 95437
      
    it "day7b" $ do
      day7b example `shouldBe` 24933642

  describe "day 8" $ do
    let example = ["30373","25512","65332","33549","35390"]
    let forest = Grid.init example
    it "day8a" $ do
      day8a example `shouldBe` 21
    it "day8b" $ do
      day8b example `shouldBe` 8
    it "surroundings" $ do
      surroundings forest (2,1) `shouldBe` [[3],[1,2],[3,5,3],[5,2]]
    it "scenicScore" $ do
      scenicScore forest (2,1) `shouldBe` 4
    it "visibleTreesInLine" $ do
      visibleTreesInLine 5 [3,5,3] `shouldBe` 2
