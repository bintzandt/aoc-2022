module Day5 where

import Aoc
import Data.List.Split
import Text.Read (readMaybe)

type Stack = [Char]

type Stacks = [Stack]

type Command = String

type Commands = [Command]

type Input = (Stacks, Commands)

unpack' :: [Maybe Int] -> [Int]
unpack' (Just a : rest) = a : unpack' rest
unpack' (Nothing : rest) = unpack' rest
unpack' [] = []

-- Moves blocks one by one, ie. reverse the blocks before adding them to the new stack.
moveBlocks :: Int -> Int -> Int -> Stacks -> Stacks
moveBlocks amount from to stacks =
  [ if i == from then drop amount s else if i == to then reverse (take amount from')++ s else s | (s, i) <- zip stacks [1 ..]]
  where from' = stacks !! (from-1)

-- Moves multiple blocks at once, ie. don't reverse the blocks before adding them to the new stack.
moveBlocks' :: Int -> Int -> Int -> Stacks -> Stacks
moveBlocks' amount from to stacks =
  [ if i == from then drop amount s else if i == to then take amount from'++ s else s | (s, i) <- zip stacks [1 ..]]
  where from' = stacks !! (from-1)

runCommand :: Command -> Stacks -> Stacks
runCommand c = moveBlocks amount from to
  where
    [amount, from, to] = unpack' . map (\s -> readMaybe s :: Maybe Int) . splitOn " " $ c

runCommand' :: Command -> Stacks -> Stacks
runCommand' c = moveBlocks' amount from to
  where
    [amount, from, to] = unpack' . map (\s -> readMaybe s :: Maybe Int) . splitOn " " $ c

head' :: Stacks -> String
head' = foldr ((:) . head) ""

getLine' :: Int -> [String] -> String
getLine' i = foldr ((:) . (!! i)) ""

parseStacks :: String -> Stacks
parseStacks s = map (filter (/= ' ')) $ [getLine' (1 + (4 * i)) stackList | i <- [0..numberOfStacks-1]]
  where
    stackList = init . lines $ s
    numberOfStacks = ((length . head $ stackList) + 1) `div` 4

task1 :: Input -> String
task1 (stacks, commands) = head' $ foldl (flip runCommand) stacks commands

task2 :: Input -> String
task2 (stacks, commands) = head' $ foldl (flip runCommand') stacks commands

prepareInput :: String -> Input
prepareInput s = (parseStacks . head $ split, lines . last $ split)
  where split = splitOn "\n\n" s

solve :: IO ()
solve = do
  input <- getPuzzleInput 5
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)
