module Day4Spec where

import Test.Hspec
import Aoc

import qualified Day4

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 2" $ do
      input <- getTestPuzzleInput 4
      let preparedInput = Day4.prepareInput input
      return (Day4.task1 preparedInput) `shouldReturn` 2

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 4" $ do
      input <- getTestPuzzleInput 4
      let preparedInput = Day4.prepareInput input
      return (Day4.task2 preparedInput) `shouldReturn` 4
