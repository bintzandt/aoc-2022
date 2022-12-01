module Main (main) where

import System.Environment (getArgs)
import Control.Monad (forM_)

import qualified Day1

solve :: Int -> IO ()
solve day
  -- prints all days
  | day == 0 = mapM_ solve [1..31]
  | day == 1 = putStrLn "** Day 1 **" >> Day1.solve
  | otherwise = error $ "No solution for day " <> show day

main :: IO ()
main = do
  args <- getArgs
  if null args
  then putStrLn "Missing argument 'day:int'"
  else solve (read $ head args)