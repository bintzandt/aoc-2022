module Day11 where

import Aoc
import Debug.Trace (trace)
import Data.Void
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import qualified Data.Map as M
import Data.List

data Monkey = Monkey {
  number :: Int,
  items :: [Int],
  operation :: Int -> Int,
  test :: Int -> Int,
  divisor :: Int,
  inspections :: Int
}

type Input = M.Map Int Monkey
type Result = Int

type Parser = Parsec Void String

monkey :: Parser Monkey
monkey = do
  number <- string "Monkey " *> decimal <* string ":" <* space1
  items <- string "Starting items: " *> sepBy1 decimal (string ", ") <* space1
  op <- string "Operation: new = old " *> (opToFn <$> asciiChar) <* char ' '
  rhs <- manyTill asciiChar space1
  divisor <- string "Test: divisible by " *> decimal <* space1
  true <- string "If true: throw to monkey " *> decimal <* space1
  false <- string "If false: throw to monkey " *> decimal
  return Monkey
    { number = number
    , items = items
    , operation = \old -> old `op` (if rhs == "old" then old else read rhs)
    , divisor = divisor
    , test = \worry -> if worry `mod` divisor == 0 then true else false
    , inspections = 0
    }
  where
    opToFn '+' = (+)
    opToFn '*' = (*)
    opToFn _ = undefined

monkeys :: Parser [Monkey]
monkeys = sepEndBy1 monkey space1

runItem :: (Int -> Int) -> Monkey -> M.Map Int Monkey -> Int -> M.Map Int Monkey
runItem update m monkeys item = 
  let worry = update $ operation m item
      target = test m worry
  in M.adjust (\m' -> m' { items = items m' ++ [worry]}) target monkeys

runMonkey update monkeys n = M.insert
  n
  m { items = [], inspections = inspections m + length (items m)}
  (foldl (runItem update m) monkeys (items m))
  where m = monkeys M.! n

runRound update monkeys =
  foldl (runMonkey update) monkeys [0..M.size monkeys-1]

monkeyBusiness = product . take 2 . reverse . sort . map inspections . M.elems

task1 :: Input -> Result
task1 monkeys = monkeyBusiness $ iterate (runRound (`div` 3)) monkeys !! 20

task2 :: Input -> Result
task2 monkeys = monkeyBusiness $ iterate (runRound (`mod` divs)) monkeys !! 10000
  where divs = product $ map divisor $ M.elems monkeys

prepareInput :: String -> Input
prepareInput input = M.fromList [(number m, m) | m <- parseMonkeys input]
  where parseMonkeys s = let Right x = parse monkeys "" s in x

solve :: IO ()
solve = do
  input <- getPuzzleInput 11
  let preparedInput = prepareInput input
  printSolution (task1 preparedInput) (task2 preparedInput)