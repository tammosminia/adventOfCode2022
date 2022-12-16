module Day7 where

import Util
import Data.List
import Data.List.Split
import Debug.Trace

type FileName = String
type FileSize = Integer
data FileTree = Directory FileName [FileTree] | File FileName FileSize deriving (Eq, Show)

--instance Show FileTree where
--  show (Directory name files) = "dir " ++ name -- ++ "[" ++ (show files) ++ "]"
--  show (File name size) = name

day7a :: [String] -> Integer
day7a lines = sum $ filter (< 100000) $ dirSizes dir
  where 
    (dir, []) = parseDir lines

dirSizes :: FileTree -> [Integer]
dirSizes (File n s) = []
dirSizes dir = sizeOf dir : concatMap dirSizes files
  where
    Directory n files = dir

sizeOf :: FileTree -> FileSize
sizeOf (File n s) = s
sizeOf (Directory n fs) = sum $ map sizeOf fs

parseInput :: [String] -> FileTree
parseInput lines = ft
  where
    (ft, []) = parseDir lines

parseDir :: [String] -> (FileTree, [String])
parseDir (cd : "$ ls" : t) = ((Directory name files), cls)
  where
    [dollar, cdc, name] = splitOn " " cd
    (files, cls) = parseFiles t

parseFiles :: [String] -> ([FileTree], [String])
parseFiles [] = ([], [])
parseFiles (h : t) 
  | isPrefixOf "dir" h = parseFiles t
  | h == "$ cd .." = ([], t)
  | isPrefixOf "$ cd" h = (d : dcfs, dcls2)
  | otherwise = (parseFile h : fcfs, fcls)
    where
      (d, dcls) = parseDir (h : t)
      (dcfs, dcls2) = parseFiles dcls
      (fcfs, fcls) = parseFiles t

parseFile :: String -> FileTree
parseFile s = File name (stringToInteger size)
  where
    [size, name] = splitOn " " s
    


