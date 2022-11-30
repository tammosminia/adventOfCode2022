module Day2020_1 where

import Util
import Data.List
  
expenseReport :: [Integer] -> Integer
expenseReport l = x1 * x2
  where
    Just (x1, x2) = find sumsTo2020 (combinations l)
    sumsTo2020 (x, y) = x + y == 2020
    
