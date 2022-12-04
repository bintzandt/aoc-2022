module Day3Spec where

import Test.Hspec
import Aoc

import qualified Day3

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 157" $ do
      input <- getTestPuzzleInput 3
      let preparedInput = Day3.prepareInput input
      return (Day3.task1 preparedInput) `shouldReturn` 157

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 70" $ do
      input <- getTestPuzzleInput 3
      let preparedInput = Day3.prepareInput input
      return (Day3.task2 preparedInput) `shouldReturn` 70
