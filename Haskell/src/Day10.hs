module Day10 where

import Aoc

type Command = String
type Input = [Int]
type Result = [Int]

runCommand :: Result -> [String]  -> Result
runCommand r [] = r
runCommand r@(x:_) ("noop":xs) = runCommand (x:r) xs
runCommand r@(x:_) (c:cs) = runCommand (newX:x:r ) cs
  where newX = x + (read . last . words $ c)

printSymbol :: String -> (Int, Int) -> String
printSymbol s (cycle, register)
  | abs (register - cycle `mod` 40) <= 1 = if cycle `mod` 40 == 0 then '#':'\n':s else '#':s
  | otherwise = '.':s

task1 :: Input -> Int
task1 results = sum . map result $ [20,60,100,140,180,220]
  where
    result i = (results !! (i-1)) * i

task2 :: Input -> String
task2 = reverse . foldl printSymbol "" . zip [0..]

prepareInput :: String -> Input
prepareInput = init . reverse . runCommand [1] . lines

solve :: IO ()
solve = do
  input <- getPuzzleInput 10
  let preparedInput = prepareInput input
  putStr $ "Task 1: " ++ show (task1 preparedInput) ++ "\nTask 2:\n" ++ task2 preparedInput