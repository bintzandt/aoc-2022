module Day4 where

import Aoc
import Data.List.Split
import Data.List (union)
import Data.Char (digitToInt)

type Assignment = [Int]
type AssignmentTuple = (Assignment, Assignment)
type Input = [AssignmentTuple]

hasOverlap :: AssignmentTuple -> Bool
hasOverlap (assignment1, assignment2) = length combined == maxLength
  where
    combined = assignment1 `union` assignment2
    maxLength = max (length assignment1) (length assignment2)

hasOverlap' :: AssignmentTuple -> Bool
hasOverlap' (assignment1, assignment2) = length combined < combinedLength
  where
    combined = assignment1 `union` assignment2
    combinedLength = length assignment1 + length assignment2

prepareInput :: String -> Input
prepareInput = map ((\[[a,b],[c,d]] -> ([a..b],[c..d])) . map (map (\i -> read i :: Int) . splitOn "-") . splitOn ",") . lines

task1 :: Input -> Int
task1 = length . filter hasOverlap

task2 :: Input -> Int
task2 = length . filter hasOverlap'

solve = do
  input <- getPuzzleInput 4
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)

