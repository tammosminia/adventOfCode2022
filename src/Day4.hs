module Day4 where

import Util
import Data.List
import Data.List.Split
import Data.Char

type Section = Int
type Range = (Section, Section) -- from inclusive, to inclusive
type Pair = (Range, Range)

day4a :: [[Section]] -> Int
day4a input = count countPair $ map parsePair input
  where
    countPair (a, b) = contains a b || contains b a

day4b :: [[Section]] -> Int
day4b input = count hasOverlap $ map parsePair input

parsePair :: [Section] -> Pair
parsePair [a1, a2, b1, b2] = ((a1, a2), (b1, b2))

-- if first range fully contains second
contains :: Range -> Range -> Bool
contains (a1, a2) (b1, b2) = a1 <= b1 && a2 >= b2

hasOverlap :: Pair -> Bool
hasOverlap ((a1, a2), (b1, b2)) = a1 <= b2 && a2 >= b1
