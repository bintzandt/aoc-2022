module Day5Spec where

import Test.Hspec
import Aoc

import qualified Day5

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in CMZ" $ do
      input <- getTestPuzzleInput 5
      let preparedInput = Day5.prepareInput input
      return (Day5.task1 preparedInput) `shouldReturn` "CMZ"

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in MCD" $ do
      input <- getTestPuzzleInput 5
      let preparedInput = Day5.prepareInput input
      return (Day5.task2 preparedInput) `shouldReturn` "MCD"