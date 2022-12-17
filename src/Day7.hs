module Day7 where

import Util
import Data.List
import Data.List.Split
import Debug.Trace
import Control.Monad.State


type InputLine = String
type Input = [InputLine]
type FileName = String
type FileSize = Integer
data FileTree = Directory FileName [FileTree] | File FileName FileSize deriving (Eq, Show)

--instance Show FileTree where
--  show (Directory name files) = "dir " ++ name -- ++ "[" ++ (show files) ++ "]"
--  show (File name size) = name

day7a :: [String] -> Integer
day7a lines = sum $ filter (< 100000) $ dirSizes $ parseInput lines

dirSizes :: FileTree -> [Integer]
dirSizes (File n s) = []
dirSizes dir = sizeOf dir : concatMap dirSizes files
  where
    Directory n files = dir

sizeOf :: FileTree -> FileSize
sizeOf (File n s) = s
sizeOf (Directory n fs) = sum $ map sizeOf fs

parseFile :: InputLine -> FileTree
parseFile s = File name (stringToInteger size)
  where
    [size, name] = splitOn " " s

-- non-monad way of parsing, returning the rest of the input along with the result

--parseInput :: Input -> FileTree
--parseInput lines = ft
--  where
--    (ft, []) = parseDir lines
--
--parseDir :: Input -> (FileTree, Input)
--parseDir (cd : "$ ls" : t) = ((Directory name files), cls)
--  where
--    [dollar, cdc, name] = splitOn " " cd
--    (files, cls) = parseFiles t
--
--parseFiles :: Input -> ([FileTree], Input)
--parseFiles [] = ([], [])
--parseFiles (h : t)
--  | isPrefixOf "dir" h = parseFiles t
--  | h == "$ cd .." = ([], t)
--  | isPrefixOf "$ cd" h = (d : dcfs, dcls2)
--  | otherwise = (parseFile h : fcfs, fcls)
--    where
--      (d, dcls) = parseDir (h : t)
--      (dcfs, dcls2) = parseFiles dcls
--      (fcfs, fcls) = parseFiles t

-- with State monad
--readLine :: State Input InputLine
--readLine = State (\(x:xs) -> (x,xs))

readLine :: State Input InputLine
readLine = do
  s <- get
  case (null s) of
    True -> return ("end")
    False -> do
      put $ tail s
      return $ head s

parseInput :: Input -> FileTree
parseInput (h : t) = evalState (parseDir h) t

parseDir :: InputLine -> State Input FileTree
parseDir cdLine = do
  ls <- readLine
  files <- parseFiles
  return (Directory name files)
  where
    [dollar, cdc, name] = splitOn " " cdLine

parseFiles :: State Input [FileTree]
parseFiles = do
  line <- readLine
  parse line
  where
    parse line
      | isPrefixOf "dir" line = parseFiles
      | line == "$ cd .." = return []
      | line == "end" = return []
      | isPrefixOf "$ cd" line = do
        dir <- parseDir line
        rest <- parseFiles
        return (dir : rest)
      | otherwise = do
        rest <- parseFiles
        return (parseFile line : rest)

