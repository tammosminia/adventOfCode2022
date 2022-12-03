module Day3 where

import Util
import Data.List
import Data.List.Split
import Data.Char

type Item = Char
type Rucksack = ([Item], [Item])
type ElfGroup = [Rucksack] -- always 3 Rucksacks

day3a :: [String] -> Int
day3a input = sum $ map (rScore . parseLine) input 
  where
    rScore r = itemPriority i
      where
        Just i = sharedItemType r

day3b :: [String] -> Int
day3b input = sum badges
  where
    rs = map parseLine input
    groups = divideElfs rs
    badges = map (itemPriority . groupBadge) groups

parseLine :: String -> Rucksack
parseLine line = splitAt (length line `div` 2) line

itemPriority :: Item -> Int
itemPriority c
  | isAsciiLower c = ord c - ord 'a' + 1
  | isAsciiUpper c = ord c - ord 'A' + 27

sharedItemType :: Rucksack -> Maybe Item  
sharedItemType (l, r) = find inRight l
  where 
    inRight i = elem i r
    
itemsInRucksack :: Rucksack -> [Item]
itemsInRucksack (l, r) = l ++ r
  
groupBadge :: ElfGroup -> Item
groupBadge g = badge
  where
    [e1, e2, e3] = map (nub . itemsInRucksack) g
    [badge] = intersect e3 $ intersect e1 e2

divideElfs :: [Rucksack] -> [ElfGroup]
divideElfs [] = []
divideElfs rs = take 3 rs : divideElfs (drop 3 rs)
