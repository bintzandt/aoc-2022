module Day6Spec where

import Test.Hspec
import Aoc

import qualified Day6

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 7" $ do
      input <- getTestPuzzleInput 6
      let preparedInput = Day6.prepareInput input
      return (Day6.task1 preparedInput) `shouldReturn` 7

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 19" $ do
      input <- getTestPuzzleInput 6
      let preparedInput = Day6.prepareInput input
      return (Day6.task2 preparedInput) `shouldReturn` 19