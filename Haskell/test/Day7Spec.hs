module Day7Spec where

import Test.Hspec
import Aoc

import qualified Day7

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 95437" $ do
      input <- getTestPuzzleInput 7
      let preparedInput = Day7.prepareInput input
      return (Day7.task1 preparedInput) `shouldReturn` 95437

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 24933642" $ do
      input <- getTestPuzzleInput 7
      let preparedInput = Day7.prepareInput input
      return (Day7.task2 preparedInput) `shouldReturn` 24933642