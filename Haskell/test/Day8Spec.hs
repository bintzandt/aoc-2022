module Day8Spec where

import Test.Hspec
import Aoc

import Day8

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 21" $ do
      input <- getTestPuzzleInput 8
      let preparedInput = prepareInput input
      return (task1 preparedInput) `shouldReturn` 21

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 8" $ do
      input <- getTestPuzzleInput 8
      let preparedInput = prepareInput input
      return (task2 preparedInput) `shouldReturn` 8