module Main (main) where

import System.Environment (getArgs)
import Control.Monad (forM_)

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5

solve :: Int -> IO ()
solve day
  -- prints all days
  | day == 0 = mapM_ solve [1..25]
  | day == 1 = putStrLn "** Day 1 **" >> Day1.solve
  | day == 2 = putStrLn "** Day 2 **" >> Day2.solve
  | day == 3 = putStrLn "** Day 3 **" >> Day3.solve
  | day == 4 = putStrLn "** Day 4 **" >> Day4.solve
  | day == 5 = putStrLn "** Day 5 **" >> Day5.solve
  | otherwise = error $ "No solution for day " <> show day

main :: IO ()
main = do
  args <- getArgs
  if null args
  then putStrLn "Missing argument 'day:int'"
  else solve (read $ head args)
