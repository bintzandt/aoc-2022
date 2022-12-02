module Day2 where

import Data.List.Split
import Aoc

type Input = [String]

prepareInput :: String -> Input
prepareInput = lines

calculateScore :: String -> Int
calculateScore (a:_:b:[])
  | a == 'A' && b == 'X' = 3 + pointsForChoice
  | a == 'A' && b == 'Y' = 6 + pointsForChoice
  | a == 'A' && b == 'Z' = 0 + pointsForChoice
  | a == 'B' && b == 'X' = 0 + pointsForChoice
  | a == 'B' && b == 'Y' = 3 + pointsForChoice
  | a == 'B' && b == 'Z' = 6 + pointsForChoice
  | a == 'C' && b == 'X' = 6 + pointsForChoice
  | a == 'C' && b == 'Y' = 0 + pointsForChoice
  | a == 'C' && b == 'Z' = 3 + pointsForChoice
  where
    pointsForChoice
      | b == 'X' = 1
      | b == 'Y' = 2
      | b == 'Z' = 3

calculateScore' :: String -> Int
calculateScore' (a:_:b:[])
  | a == 'A' && b == 'X' = 3 + pointsForChoice
  | a == 'A' && b == 'Y' = 1 + pointsForChoice
  | a == 'A' && b == 'Z' = 2 + pointsForChoice
  | a == 'B' && b == 'X' = 1 + pointsForChoice
  | a == 'B' && b == 'Y' = 2 + pointsForChoice
  | a == 'B' && b == 'Z' = 3 + pointsForChoice
  | a == 'C' && b == 'X' = 2 + pointsForChoice
  | a == 'C' && b == 'Y' = 3 + pointsForChoice
  | a == 'C' && b == 'Z' = 1 + pointsForChoice
  where
    pointsForChoice
      | b == 'X' = 0
      | b == 'Y' = 3
      | b == 'Z' = 6

task1 :: Input -> Int
task1 = sum . map calculateScore

task2 :: Input -> Int
task2 = sum . map calculateScore'

solve = do
  input <- getPuzzleInput 2
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)
