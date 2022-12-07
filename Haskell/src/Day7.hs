module Day7 where

import Aoc
import qualified Data.Map as M

type Directory = String
type Files = [String]
type Commands = [String]
type FileSystem = ([Directory], M.Map Directory Files)
type Input = M.Map Directory Int

-- Version of unwords that adds a '/' instead of a space
unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:xs) = x ++ "/" ++ unwords' xs

addToFileSystem :: FileSystem -> Files -> FileSystem
addToFileSystem (dir, fs) files = (dir, M.union fs (M.fromList [(unwords' dir, files)]))

processCommands :: FileSystem -> Commands -> FileSystem
processCommands fs [] = fs
processCommands (dir, files) ("$ cd ..":c) = processCommands (init dir, files) c
processCommands (dir, files) (('$':' ':'c':'d':' ':r):c) = processCommands (dir ++ [r], files) c
processCommands fs ("$ ls":c) = processCommands (addToFileSystem fs files) commands
  where
    files = takeWhile (\co -> head co /= '$') c
    commands = dropWhile (\co -> head co /= '$')  c

getSum :: M.Map Directory Files -> Directory -> Files -> Int
getSum com d = sum . map (\file -> case file of
  'd':'i':'r':' ':d' -> getSum com (d++d'++"/") (com M.! (d++d'++"/"))
  _ -> read . head . words $ file
  )

task1 :: Input -> Int
task1 = M.foldr (+) 0 . M.filter (< 100000)

task2 :: Input -> Int
task2 i = do
  let totalSpace = 70000000
  let totalNeededSpace = 30000000
  let availableSpace = totalSpace - (i M.! "//")
  let toFreeUpSpace = totalNeededSpace - availableSpace
  minimum . map snd . M.toList . M.filter (>toFreeUpSpace) $ i

prepareInput :: String -> Input
prepareInput i = do
  let fs = snd . processCommands (["/"], M.empty) . drop 1 . lines $ i
  M.mapWithKey (getSum fs) fs

solve :: IO ()
solve = do
  input <- getPuzzleInput 7
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)