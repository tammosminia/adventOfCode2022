module Day8 where

import Util
import qualified Grid

type Height = Int
type Forest = Grid.Grid Height

day8a :: [String] -> Int
day8a input = count (isVisible forest) (Grid.allPoints forest)
  where
    forest = Grid.init input

day8b :: [String] -> Int
day8b input = maximum $ map (scenicScore forest) $ Grid.allPoints forest
  where
    forest = Grid.init input

isVisible :: Forest -> Grid.Point -> Bool
isVisible forest p = any visibleFrom $ surroundings forest p
  where
    height = Grid.get forest p
    (maxX, maxY) = Grid.bottomRight forest
    visibleFrom treesInFront = all (< height) treesInFront

surroundings :: Forest -> Grid.Point -> [[Height]]
surroundings forest p = map heightsInDirection Grid.allDirections
  where
    heightsInDirection = map (Grid.get forest) . Grid.pointsInDirection forest p

scenicScore :: Forest -> Grid.Point -> Int
scenicScore forest p = product $ map (visibleTreesInLine height) $ surroundings forest p
  where
    height = Grid.get forest p

visibleTreesInLine :: Height -> [Height] -> Int
visibleTreesInLine height trees = length $ takeUntil (>= height) trees
    
    