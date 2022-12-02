module Day2 where

import Util
import Data.List
import Data.List.Split

data Choice = Rock | Paper | Scissors deriving (Show, Eq, Ord)
data Result = Loose | Draw | Win deriving (Show, Eq, Ord)
  
day2a :: [String] -> Integer
day2a input = sum $ map (lineScore . parseLine) input
  where
    lineScore (a, b) = shapeScore b + winScore (a, b)
    
day2b :: [String] -> Integer
day2b input = sum $ map (lineScore . parseLine2) input
  where
    lineScore (a, r) = shapeScore b + winScore (a, b)
      where
        b = whatToDo (a, r)
    
parseLine :: String -> (Choice, Choice)
parseLine line = (parseAbc a, parseXyz b)
  where
    [a, b] = splitOn " " line

parseLine2 :: String -> (Choice, Result)
parseLine2 line = (parseAbc a, parseXyz2 b)
  where
    [a, b] = splitOn " " line

parseAbc :: String -> Choice
parseAbc "A" = Rock
parseAbc "B" = Paper
parseAbc "C" = Scissors

parseXyz :: String -> Choice
parseXyz "X" = Rock
parseXyz "Y" = Paper
parseXyz "Z" = Scissors

parseXyz2 :: String -> Result
parseXyz2 "X" = Loose
parseXyz2 "Y" = Draw
parseXyz2 "Z" = Win

shapeScore :: Choice -> Integer
shapeScore Rock = 1
shapeScore Paper = 2
shapeScore Scissors = 3
  
winScore :: (Choice, Choice) -> Integer
winScore (Rock, Paper) = 6
winScore (Paper, Scissors) = 6
winScore (Scissors, Rock) = 6
winScore (opponent, own)
  | opponent == own = 3
  | otherwise = 0

whatToDo :: (Choice, Result) -> Choice
whatToDo (x, Draw) = x
whatToDo (Rock, Win) = Paper
whatToDo (Paper, Win) = Scissors
whatToDo (Scissors, Win) = Rock
whatToDo (Rock, Loose) = Scissors
whatToDo (Paper, Loose) = Rock
whatToDo (Scissors, Loose) = Paper
  
