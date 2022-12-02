module Day2Spec where

import Test.Hspec
import Aoc

import qualified Day2

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 15" $ do
      input <- getTestPuzzleInput 2
      let preparedInput = Day2.prepareInput input
      (return $ Day2.task1 preparedInput) `shouldReturn` 15

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 12" $ do
      input <- getTestPuzzleInput 2
      let preparedInput = Day2.prepareInput input
      (return $ Day2.task2 preparedInput) `shouldReturn` 12
