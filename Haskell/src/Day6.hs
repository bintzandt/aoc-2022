module Day6 where

import Aoc

type Input = String

anySame :: Eq a => [a] -> Bool
anySame [] = False
anySame (a:rest) = a `elem` rest || anySame rest

firstUniqueIndex :: Eq a => Int -> [a] -> Int
firstUniqueIndex = firstUniqueIndex' 0
  where
    firstUniqueIndex' i l list
      | not . anySame . take l $ list = i + l
      | otherwise = firstUniqueIndex' (i+1) l (drop 1 list)

task1 :: Input -> Int
task1 = firstUniqueIndex 4

task2 :: Input -> Int
task2 = firstUniqueIndex 14

prepareInput :: String -> Input
prepareInput = id

solve :: IO ()
solve = do
  input <- getPuzzleInput 6
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)