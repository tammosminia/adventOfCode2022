module Day2020_1 where

import Util
import Data.List
  
expenseReport :: [Integer] -> Integer
expenseReport l = x1 * x2
  where
    Just [x1, x2] = find sumsTo2020 (combinations 2 l)
    sumsTo2020 [x, y] = x + y == 2020
    
expenseReport2 :: [Integer] -> Integer
expenseReport2 l = x1 * x2 * x3
  where
    Just [x1, x2, x3] = find sumsTo2020 (combinations 3 l)
    sumsTo2020 [x, y, z] = x + y + z == 2020
    
