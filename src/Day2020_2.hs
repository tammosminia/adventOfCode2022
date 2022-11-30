module Day2020_2 where

import Util
import Data.List
import Data.List.Split

validPassword :: String -> Bool
validPassword line = (policyMin <= letterCount) && (letterCount <= policyMax)
  where
    [policy, password] = splitOn ": " line
    [policyAmounts, policyLetterS] = splitOn " " policy
    [policyLetter] = policyLetterS
    [policyMin, policyMax] = map stringToInt $ splitOn "-" policyAmounts
    letterCount = count (==policyLetter) password
    
validPassword2 :: String -> Bool
validPassword2 line = (count (policyLetter==) letters) == 1
  where
    [policy, password] = splitOn ": " line
    [policyAmounts, policyLetterS] = splitOn " " policy
    [policyLetter] = policyLetterS
    posl = map (\x -> x - 1) $ map stringToInt $ splitOn "-" policyAmounts
    letters = map (\x -> password !! x) posl
    
    