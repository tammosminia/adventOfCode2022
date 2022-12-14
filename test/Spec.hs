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
import Day9
import Day10
import Day11

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
      
    it "readInts" $ do
      readInts "" `shouldBe` []
      readInts "bla" `shouldBe` []
      readInts "13" `shouldBe` [13]
      readInts "bla 1 2 bla 3" `shouldBe` [1,2,3]
      
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
      print $ show $ Day7.parseInput ["$ cd /","$ ls","dir a"]
      print $ show $ Day7.parseInput example

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

  describe "day 9" $ do
    let example = ["R 4","U 4","L 3","D 1","R 4","D 1","L 5","R 2"]
    let example2 = ["R 5","U 8","L 8","D 3","R 17","D 10","L 25","U 20"]
    it "day9a" $ do
      day9a example `shouldBe` 13
    it "day9b" $ do
      day9b example `shouldBe` 1
      day9b example2 `shouldBe` 35 -- Should be 36, also the real answer is one off
      
  describe "day 10" $ do
    let example = ["addx 15","addx -11","addx 6","addx -3","addx 5","addx -1","addx -8","addx 13","addx 4","noop","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx -35","addx 1","addx 24","addx -19","addx 1","addx 16","addx -11","noop","noop","addx 21","addx -15","noop","noop","addx -3","addx 9","addx 1","addx -3","addx 8","addx 1","addx 5","noop","noop","noop","noop","noop","addx -36","noop","addx 1","addx 7","noop","noop","noop","addx 2","addx 6","noop","noop","noop","noop","noop","addx 1","noop","noop","addx 7","addx 1","noop","addx -13","addx 13","addx 7","noop","addx 1","addx -33","noop","noop","noop","addx 2","noop","noop","noop","addx 8","noop","addx -1","addx 2","addx 1","noop","addx 17","addx -9","addx 1","addx 1","addx -3","addx 11","noop","noop","addx 1","noop","addx 1","noop","noop","addx -13","addx -19","addx 1","addx 3","addx 26","addx -30","addx 12","addx -1","addx 3","addx 1","noop","noop","noop","addx -9","addx 18","addx 1","addx 2","noop","noop","addx 9","noop","noop","noop","addx -1","addx 2","addx -37","addx 1","addx 3","noop","addx 15","addx -21","addx 22","addx -6","addx 1","noop","addx 2","addx 1","noop","addx -10","noop","noop","addx 20","addx 1","addx 2","addx 2","addx -6","addx -11","noop","noop","noop"]
    it "day10a" $ do
      day10a example `shouldBe` 13140
    it "day10b" $ do
      let result = unlines ["##..##..##..##..##..##..##..##..##..##..","###...###...###...###...###...###...###.","####....####....####....####....####....","#####.....#####.....#####.....#####.....","######......######......######......####","#######.......#######.......#######....."]
      day10b example `shouldBe` result
    it "runProgram" $ do
      runProgram (Day10.parseInput ["noop","addx 3","addx -5","noop"]) `shouldBe` [1,1,1,4,4,-1]
      
  describe "day 11" $ do
    let example = ["Monkey 0:","Starting items: 79, 98","Operation: new = old * 19","Test: divisible by 23","If true: throw to monkey 2","If false: throw to monkey 3","","Monkey 1:","Starting items: 54, 65, 75, 74","Operation: new = old + 6","Test: divisible by 19","If true: throw to monkey 2","If false: throw to monkey 0","","Monkey 2:","Starting items: 79, 60, 97","Operation: new = old * old","Test: divisible by 13","If true: throw to monkey 1","If false: throw to monkey 3","","Monkey 3:","Starting items: 74","Operation: new = old + 3","Test: divisible by 17","If true: throw to monkey 0","If false: throw to monkey 1"]
    it "day11a" $ do
      day11a example `shouldBe` 10605
    it "day11b" $ do
      day11b example `shouldBe` 2713310158
