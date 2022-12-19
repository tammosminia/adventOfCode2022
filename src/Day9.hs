module Day9 where

import Data.List
import Data.List.Split
import Control.Monad.State
import Util
import qualified Grid

data Move = Move Grid.Direction Int
data Rope = Rope { path :: [Grid.Point], h :: Grid.Point, t :: Grid.Point }
type RopeState = State Rope

day9a :: [String] -> Int
day9a input = length $ nub path
  where
    initState = Rope [] (0,0) (0,0)
    Rope path h t = execState (go (parseInput input)) initState

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
rememberPoint p = do
  Rope path h t <- get
  put (Rope (p:path) h t)
  
getHt :: RopeState (Grid.Point, Grid.Point)
getHt = do
  Rope path h t <- get
  return (h, t)

putHt :: (Grid.Point, Grid.Point) -> RopeState ()
putHt (h, t) = do
  Rope path _ _ <- get
  put (Rope path h t)

touches :: Grid.Point -> Grid.Point -> Bool
touches (hx, hy) (tx, ty) = (abs (hx - tx) < 2) && (abs (hy - ty) < 2)

go :: [Move] -> RopeState ()
go [] = return ()
go ((Move d 0):ms) = go ms
go ((Move d n):ms) = do
  (h, t) <- getHt
  let newH = Grid.move d h
  let newT = (if touches newH t then t else h)
  putHt (newH, newT)
  rememberPoint newT
  go (Move d (n-1):ms)
  
