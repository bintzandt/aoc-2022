module Day9 where

import Aoc
import Debug.Trace (trace)
import qualified Data.Set as S

data Direction = U | R | D | L
type Coordinate = (Int, Int)
type Move = (Direction, Int)
type Input = [Coordinate]

runCommand :: Move -> Coordinate -> (Coordinate, [Coordinate])
runCommand (R, l) (x,y) = ((x+l, y), zip [x+1..x+l] (repeat y))
runCommand (D, l) (x,y) = ((x, y+l), zip (repeat x) [y+1..y+l])
runCommand (L, l) (x,y) = ((x-l, y), zip (reverse [x-l..x-1]) (repeat y))
runCommand (U, l) (x,y) = ((x, y-l), zip (repeat x) (reverse [y-l..y-1]))

prepareInput :: String -> Input
prepareInput = snd . foldl (\(c, list) move -> let (newC, newList) = runCommand move c in (newC, list ++ newList)) ((0,0), [(0,0)]) . map parseLine . lines
  where
    parseLine ('R':' ':l) = (R, read l)
    parseLine ('D':' ':l) = (D, read l)
    parseLine ('L':' ':l) = (L, read l)
    parseLine ('U':' ':l) = (U, read l) 

shouldSkip :: Coordinate -> Coordinate -> Bool
shouldSkip (x1, y1) (x2, y2) = abs (y1 - y2) <= 1 && abs (x1 - x2) <= 1

removeSkippedCoordinates :: [Coordinate] -> [Coordinate]
removeSkippedCoordinates = reverse . foldl update []
  where
    update [] v = [v]
    update pos@(c@(x', y'):_) c'@(x,y)
      | shouldSkip c c' = pos
      | otherwise = (x' + signum (x-x'), y' + signum (y-y')):pos  

task1 :: Input -> Int
task1 = S.size . S.fromList . removeSkippedCoordinates

task2 :: Input -> Int
task2 = S.size . S.fromList . flip (!!) 9 . iterate removeSkippedCoordinates

solve :: IO ()
solve = do
  input <- getPuzzleInput 9
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)