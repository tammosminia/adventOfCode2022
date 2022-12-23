module Day11 where

import Util
import Data.List
import Data.List.Split
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State

type MonkeyId = Int
type Item = Integer
type Activity = Integer
data Monkey = Monkey { getId :: MonkeyId, startItems :: [Item], operation :: Item -> Item, test :: Item -> Bool, toMonkey :: Bool -> MonkeyId }
type ZooState = State (Map.Map MonkeyId ([Item], Activity))
type DivideWorries = Bool

day11a :: [String] -> Integer
day11a = day11 True 20

day11b :: [String] -> Integer
day11b = day11 False 10000

day11 :: DivideWorries -> Int -> [String] -> Integer
day11 di numRounds input = product $ take 2 $ reverse $ sort activityScores
  where
    monkeys = map parseMonkey $ splitOn [""] input
    startState = Map.fromList $ map monkeyStartState monkeys
    monkeyStartState m = (getId m, (startItems m, 0))
    rounds = replicateM_ numRounds $ monkeyRound di monkeys
    s = execState rounds startState
    activityScores = map snd $ Map.elems s

parseMonkey :: [String] -> Monkey
parseMonkey [idLine, itemLine, opLine, testLine, trueLine, falseLine] = Monkey id items operation test toMonkey
  where
    readIntegers line = map toInteger $ readInts line
    [id] = readInts idLine
    items = readIntegers itemLine
    opFun = case opLine !! 21 of
      '+' -> (+)
      '*' -> (*)
    opNum = listToMaybe $ readIntegers opLine
    operation x = opFun x $ fromMaybe x opNum
    [testNum] = readIntegers testLine
    test x = mod x testNum == 0
    [trueMonkey] = readInts trueLine
    [falseMonkey] = readInts falseLine
    toMonkey True = trueMonkey
    toMonkey False = falseMonkey

takeItems :: MonkeyId -> ZooState [Item]
takeItems mid = do
  s <- get
  let update (items, activity) = ([], activity + toInteger (length items))
  put (Map.adjust update mid s)
  return (fst (s Map.! mid))

addItem :: MonkeyId -> Item -> ZooState ()
addItem mid item = modify update
  where
    update s = Map.adjust updateMonkey mid s
    updateMonkey (items, activity) = (items ++ [item], activity) 

monkeyTurn :: DivideWorries -> Monkey -> ZooState ()
monkeyTurn di monkey = do
  items <- takeItems (getId monkey)
  mapM (throwItem monkey) $ map (inspectItem di monkey) items
  return ()

inspectItem :: DivideWorries -> Monkey -> Item -> Item
inspectItem True m i = div ((operation m) i) 3
inspectItem False m i = (operation m) i

throwItem :: Monkey -> Item -> ZooState ()
throwItem m i = do
  let tr = (test m) i
  let to = (toMonkey m) tr
  addItem to i

monkeyRound :: DivideWorries -> [Monkey] -> ZooState ()
monkeyRound di = mapM_ (monkeyTurn di)