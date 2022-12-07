module Day3 where

import Aoc
import Data.Char
import Data.List.Split

type Item = Char
type Items = [Item]
type Knapsack = (Items, Items)
type Input = [String]

prepareInput :: String -> Input
prepareInput = lines

priority :: Item -> Int
priority i
  -- Lowercase items (ascii codes 97-122)
  | o >= 97 = o - ord 'a' + 1
  -- Uppercase items (ascii codes 65-90)
  | otherwise = o - ord 'A' + 27
  where o = ord i

-- This function will cause an non-exaustive pattern error if there is no common item.
-- It is not a problem since the input is guaranteed to have a common item.
findCommonItem :: Knapsack -> Item
findCommonItem (i:items, second) = if i `elem` second then i else findCommonItem (items, second)

findCommonItem' :: [Items] -> Item
findCommonItem' [i:a, b, c] = if i `elem` b && i `elem` c then i else findCommonItem' [a,b,c]

task1 :: Input -> Int
task1 = sum . map (priority . findCommonItem . (\line -> splitAt (length line `div` 2) line))

task2 :: Input -> Int
task2 = sum . map (priority . findCommonItem') . chunksOf 3

solve :: IO ()
solve = do
  input <- getPuzzleInput 3
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)

