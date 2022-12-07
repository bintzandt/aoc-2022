module Day1 where

import Data.List.Split
import Data.List (sort)
import Aoc


task1 :: [Int] -> Int
task1 = maximum

task2 :: [Int] -> Int
task2 = sum . take 3 . reverse . sort

-- splitOn variant that also drops blanks.
splitOn' :: [Char] -> [Char] -> [[Char]]
splitOn' = split . dropDelims . dropBlanks . onSublist

prepareInput :: String -> [Int]
prepareInput = map (\list -> sum $ map read list) . map lines . splitOn' "\n\n"

solve :: IO ()
solve = do
  input <- getPuzzleInput 1
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)
