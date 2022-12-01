module Day1 where

import Util
import Data.List
  
day1a :: [[Integer]] -> Integer
day1a input = maximum perElf
  where
    perElf = map sum input
    
day1b :: [[Integer]] -> Integer
day1b input = sum $ take 3 $ reverse $ sort perElf
  where
    perElf = map sum input
    
