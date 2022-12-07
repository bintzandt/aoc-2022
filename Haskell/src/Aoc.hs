module Aoc (
    printSolution,
    getPuzzleInput,
    getTestPuzzleInput
) where

printSolution :: Show a => Show b => a -> b -> IO ()
printSolution a b = putStrLn $ "Task 1: " ++ show a  ++ "\nTask 2: " ++ show b

getPuzzleInput :: Int -> IO String
getPuzzleInput day = readFile ("puzzle-inputs/day" ++ show day ++ ".txt")

getTestPuzzleInput :: Int -> IO String
getTestPuzzleInput day = readFile ("puzzle-inputs/day" ++ show day ++ ".test.txt")
