module Day5 where

import Util
import Data.List
import Data.List.Split
import qualified Data.Map as Map

type StackId = Int
type Stack = String -- starting at the top
type Stacks = Map.Map StackId Stack
type Move = (Int, StackId, StackId) -- how many, from, to
type MoveMethod = Move -> Stacks -> Stacks

day5a :: [String] -> [String] -> String
day5a iStacks iMoves = tops $ moves moveA (map parseMove iMoves) (parseStacks iStacks)
  
day5b :: [String] -> [String] -> String
day5b iStacks iMoves = tops $ moves moveB (map parseMove iMoves) (parseStacks iStacks)
  
parseStacks :: [String] -> Stacks
parseStacks input = Map.fromList $ zip [1..9] input

parseMove :: String -> Move
parseMove line = (stringToInt m, stringToInt f, stringToInt t)
  where
    [move, m, from, f, to, t] = splitOn " " line

moveA :: Move -> Stacks -> Stacks
moveA (0, f, t) stacks = stacks
moveA (x, f, t) stacks = moveA (x-1, f, t) moved1
  where
    Just (fromTop : fromBottom) = Map.lookup f stacks
    Just toBottom = Map.lookup t stacks
    moved1 = Map.insert t (fromTop : toBottom) $ Map.insert f fromBottom stacks
    
moveB :: Move -> Stacks -> Stacks
moveB (x, f, t) stacks = Map.insert t (fromTop ++ toBottom) $ Map.insert f fromBottom stacks
  where
    Just fromStack = Map.lookup f stacks
    (fromTop, fromBottom) = splitAt x fromStack
    Just toBottom = Map.lookup t stacks
    
moves :: MoveMethod -> [Move] -> Stacks -> Stacks
moves mm [] stacks = stacks
moves mm (m: ms) stacks = moves mm ms $ mm m stacks

tops :: Stacks -> String
tops stacks = concatMap top [1..9]
  where
    top :: StackId -> String
    top id = case Map.lookup id stacks of
      Just stack -> take 1 stack
      Nothing -> []
