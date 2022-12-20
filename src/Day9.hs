module Day9 where

import Data.List
import Data.List.Split
import Control.Monad.State
import Debug.Trace
import Util
import qualified Grid

data Move = Move Grid.Direction Int
data Rope = Rope { path :: [Grid.Point], hts :: [Grid.Point] }
type RopeState = State Rope

day9a :: [String] -> Int
day9a = runDay 1

day9b :: [String] -> Int
day9b = runDay 9

runDay :: Int -> [String] -> Int
runDay knots input = length $ nub path
  where
    initState = Rope [] $ replicate (knots+1) (0,0)
    Rope path _ = execState (go (parseInput input)) initState

parseInput :: [String] -> [Move]
parseInput = map parseLine
  where
    parseLine line = case splitOn " " line of
      [d, n] -> Move (parseDirection d) (stringToInt n)
    parseDirection "U" = Grid.North
    parseDirection "R" = Grid.East
    parseDirection "D" = Grid.South
    parseDirection "L" = Grid.West
    
rememberPoint :: Grid.Point -> RopeState ()
rememberPoint p = modify update
  where 
    update (Rope path hts) = Rope (p:path) hts
  
getHts :: RopeState [Grid.Point]
getHts = do
  Rope path hts <- get
  return hts

putHts :: [Grid.Point] -> RopeState ()
putHts hts = do
  Rope path _ <- get
  put (Rope path hts)

touches :: Grid.Point -> Grid.Point -> Bool
touches (hx, hy) (tx, ty) = (abs (hx - tx) < 2) && (abs (hy - ty) < 2)

go :: [Move] -> RopeState ()
go [] = return ()
go ((Move d 0):ms) = go ms
go ((Move d n):ms) = do
  hts <- getHts
  let newH = Grid.move d (head hts)
  let newHts = fixHts (newH:tail hts)
  putHts newHts
  rememberPoint (last hts)
  go (Move d (n-1):ms)
  
fixHts :: [Grid.Point] -> [Grid.Point]
fixHts [x] = [x]
fixHts(h:t:ts)
  | touches h t = h:t:ts
  | otherwise = h:fixHts (moveT h t:ts)
    
moveT :: Grid.Point -> Grid.Point -> Grid.Point
moveT (hx,hy) (tx,ty) = (closerTo hx tx, closerTo hy ty)
  where
    closerTo x2 x1
      | x1 == x2 = x1
      | x1 < x2 = x1 + 1
      | otherwise = x1 - 1
