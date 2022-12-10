module Main (main) where

import System.Environment (getArgs)
import Control.Monad (forM_)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9

solve :: Int -> IO ()
solve day
  -- prints all days
  | day == 0 = mapM_ solve [1..25]
  | day == 1 = putStrLn "** Day 1 **" >> Day1.solve
  | day == 2 = putStrLn "** Day 2 **" >> Day2.solve
  | day == 3 = putStrLn "** Day 3 **" >> Day3.solve
  | day == 4 = putStrLn "** Day 4 **" >> Day4.solve
  | day == 5 = putStrLn "** Day 5 **" >> Day5.solve
  | day == 6 = putStrLn "** Day 6 **" >> Day6.solve
  | day == 7 = putStrLn "** Day 7 **" >> Day7.solve
  | day == 8 = putStrLn "** Day 8 **" >> Day8.solve
  | day == 9 = putStrLn "** Day 9 **" >> Day9.solve
  | otherwise = error $ "No solution for day " <> show day

main :: IO ()
main = do
  args <- getArgs
  if null args
  then putStrLn "Missing argument 'day:int'"
  else solve (read $ head args)
