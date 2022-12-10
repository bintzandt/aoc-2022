module Day8 where

import Aoc
import qualified Day1
import qualified Data.Map as M
import qualified Data.Set as S

type Input = (Grid, Int, Int)
type Coordinate = (Int,Int)
type Height = Int
type Grid = M.Map Coordinate Height

task1 :: Input -> Int
task1 = countVisibleTrees
  
task2 :: Input -> Int
task2 (grid, maxX, maxY) = maximum . M.elems . M.mapWithKey (countTrees (grid,maxX,maxY)) $ grid
  
countTrees :: Input -> Coordinate -> Height -> Int
countTrees (g, maxX, maxY) (x,y) h = smallerTreesInList up * smallerTreesInList down * smallerTreesInList left * smallerTreesInList right
  where
    smallerTreesInList [] = 0
    smallerTreesInList (x:xs) = if (g M.! x) >= h then 1 else 1 + smallerTreesInList xs
    up = zip (repeat x) (reverse [0..y-1])
    down = zip (repeat x) [y+1..maxY]
    left = zip (reverse [0..x-1]) (repeat y)
    right = zip [x+1..maxX] (repeat y)

isVisible :: Input -> Coordinate -> Height -> Bool
isVisible (g, maxX, maxY) (x,y) h = notHasLargerTree up || notHasLargerTree down || notHasLargerTree left || notHasLargerTree right
  where
    up = S.fromList (zip (repeat x) [0..y-1])
    down = S.fromList (zip (repeat x) [y+1..maxY])
    left = S.fromList (zip [0..x-1] (repeat y))
    right = S.fromList (zip [x+1..maxX] (repeat y))
    notHasLargerTree set = M.null . M.filter (>=h) $ g `M.restrictKeys` set

countVisibleTrees :: Input -> Int
countVisibleTrees (g, maxX, maxY) = M.size . M.filter (==True) . M.mapWithKey (isVisible (g, maxX, maxY)) $ g

listToGrid :: [[Int]] -> Int -> Grid -> Grid
listToGrid [] _ _ = M.empty
listToGrid (row:list) y g = M.union (foldl (\g (x,y,v) -> M.insert (x,y) v g) g zipped) (listToGrid list (y+1) M.empty)
  where
    zipped = zip3 [0..] (repeat y) row

prepareInput :: String -> Input
prepareInput str = (grid, maxX, maxY)
  where
      i = map (map (\x -> read x :: Int) . Day1.splitOn' "") . lines $ str
      maxY = length i - 1
      maxX = (length . head $ i) - 1
      grid = listToGrid i 0 M.empty

solve :: IO ()
solve = do
  input <- getPuzzleInput 8
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)