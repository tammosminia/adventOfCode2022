module Day10 where

import Data.List
import Data.List.Split
import Control.Monad.State
import Debug.Trace
import Util

type Register = Int
type Time = Int
data Command = Noop | AddX Register
type Output = [Register] --register value at start of the cycle
type RegisterState = State (Register, [Register]) --(current, history)
type ScreenLine = String
type Screen = String -- with "\n"s

day10a :: [String] -> Int
day10a input = sum values
  where
    commands = parseInput input
    history = runProgram commands
    interestingTimes = [20,60,100,140,180,220]
    values = map atTime interestingTimes
    atTime time = time * (history !! (time-1))

day10b :: [String] -> Screen
day10b input = renderScreen history
  where
    commands = parseInput input
    history = runProgram commands
    
renderLine :: Output -> ScreenLine
renderLine history = mapWithIndex renderPixel history
  where
    mapWithIndex f l = map (\(i, x) -> f i x) $ zip [0..] l 
    renderPixel x r = if abs (x - r) < 2 then '#' else '.'

renderScreen :: Output -> Screen
renderScreen history = unlines lines
  where
    lines = take 6 $ map renderLine $ chunksOf 40 history
 
parseInput :: [String] -> [Command]
parseInput = map parseLine
  where
    parseLine line = case splitOn " " line of
      ["noop"] -> Noop
      ["addx", x] -> AddX (read x :: Register)
  
wait :: Time -> RegisterState ()
wait time = modify update
  where
    update (current, history) = (current, history ++ replicate time current)
        
updateRegister :: (Register -> Register) -> RegisterState ()
updateRegister f = modify update
  where
    update (current, history) = (f current, history)
        
runCommand :: Command -> RegisterState ()
runCommand Noop = wait 1
runCommand (AddX x) = do
  wait 2
  updateRegister (+x)

runCommands :: [Command] -> RegisterState ()
runCommands [] = return ()
runCommands (x:xs) = do
  runCommand x
  runCommands xs
  
runProgram :: [Command] -> [Register]
runProgram cs = history
  where
    startState = (1, [])
    (_, history) = execState (runCommands cs) startState
      