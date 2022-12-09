module Day6 where

import Util
import Data.List
import Data.List.Split
import qualified Data.Map as Map


day6a :: String -> Integer
day6a input = result
  where
    start = take 4 input
    allDifferent = length (nub start) == 4
    result 
      | allDifferent = 4
      | otherwise = 1 + day6a (tail input)
      
day6b :: String -> Integer
day6b input = result
  where
    start = take 14 input
    allDifferent = length (nub start) == 14
    result 
      | allDifferent = 14
      | otherwise = 1 + day6b (tail input)
