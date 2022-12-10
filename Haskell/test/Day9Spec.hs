module Day9Spec where

import Test.Hspec
import Aoc

import Day9

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 13" $ do
      input <- getTestPuzzleInput 9
      let preparedInput = prepareInput input
      return (task1 preparedInput) `shouldReturn` 13

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 0" $ do
      input <- getTestPuzzleInput 9
      let preparedInput = prepareInput input
      return (task2 preparedInput) `shouldReturn` 0