module Day10Spec where

import Test.Hspec
import Aoc

import Day10

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 13140" $ do
      input <- getTestPuzzleInput 10
      let preparedInput = prepareInput input
      return (task1 preparedInput) `shouldReturn` 13140

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in #######" $  do
      input <- getTestPuzzleInput 10
      let preparedInput = prepareInput input
      return (task2 preparedInput) `shouldReturn` "\n##..##..##..##..##..##..##..##..##..##..\n###...###...###...###...###...###...###.\n####....####....####....####....####....\n#####.....#####.....#####.....#####.....\n######......######......######......####\n#######.......#######.......#######....."