module Day7 where

import Aoc
import qualified Data.Map as M

type Directory = [String]
type Files = [String]
type Commands = [String]
type FileSystem = (Directory, M.Map String Int)
type Input = [Int]

addToFileSystem :: FileSystem -> String -> FileSystem
addToFileSystem (dir, fs) file = do
  let size = read . head . words $ file
  let paths = drop 1 . scanl (++) "" . reverse $ dir
  let zipped = M.fromList (zip paths (repeat size))
  (dir, M.unionWith (+) fs zipped)

processCommands :: FileSystem -> Commands -> FileSystem
processCommands fs [] = fs
processCommands (dir, files) ("$ cd ..":c) = processCommands (tail dir, files) c
processCommands (dir, files) (('$':' ':'c':'d':' ':r):c) = processCommands (r:dir, files) c
-- Ignore the 'ls' command
processCommands fs ("$ ls":c) = processCommands fs c
-- Ignore directories (they will be summed up later)
processCommands fs (('d':'i':'r':_):c) = processCommands fs c
processCommands (dir, files) (a:c) = processCommands (addToFileSystem (dir, files) a) c

task1 :: Input -> Int
task1 = sum . filter (< 100000)

task2 :: Input -> Int
task2 i = do
  let totalSpace = 70000000
  let totalNeededSpace = 30000000
  let availableSpace = totalSpace - head i
  let toFreeUpSpace = totalNeededSpace - availableSpace
  minimum . filter (>toFreeUpSpace) $ i

prepareInput :: String -> Input
prepareInput = M.elems . snd . processCommands (["/"], M.empty) . drop 1 . lines

solve :: IO ()
solve = do
  input <- getPuzzleInput 7
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)