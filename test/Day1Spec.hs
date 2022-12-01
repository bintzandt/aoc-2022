module Day1Spec where

import Test.Hspec
import Aoc

import qualified Day1

main :: IO ()
main = hspec spec

spec :: Spec
spec = task1Spec >> task2Spec

task1Spec :: Spec
task1Spec = describe "Task 1" $ do
  describe "With the test puzzle input" $
    it "Results in 24000" $ do
      input <- getTestPuzzleInput 1
      let preparedInput = Day1.prepareInput input
      (return $ Day1.task1 preparedInput) `shouldReturn` 24000

task2Spec :: Spec
task2Spec = describe "Task 2" $ do
  describe "With the test puzzle input" $
    it "Results in 45000" $ do
      input <- getTestPuzzleInput 1
      let preparedInput = Day1.prepareInput input
      (return $ Day1.task2 preparedInput) `shouldReturn` 45000
