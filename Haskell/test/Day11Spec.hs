module Day11Spec where

import Test.Hspec
import Aoc

import Day11

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 10605" $ do
      input <- getTestPuzzleInput 11
      let preparedInput = prepareInput input
      return (task1 preparedInput) `shouldReturn` 10605

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 2713310158" $  do
      input <- getTestPuzzleInput 11
      let preparedInput = prepareInput input
      return (task2 preparedInput) `shouldReturn` 2713310158