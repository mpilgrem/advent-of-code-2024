module Main where

import qualified Data.List as L
import           Data.Functor ( void )
import           System.Environment ( getArgs )
import qualified Text.Parsec as Parsec

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: aoc2024 <puzzle> ..."
    (puzzle:args') -> case puzzle of
      "1A" -> puzzle1A args'
      "1B" -> puzzle1B args'
      "2A" -> puzzle2A args'
      "2B" -> puzzle2B args'
      "3A" -> puzzle3A args'
      "3B" -> puzzle3B args'
      _ -> putStrLn $ "Unknown puzzle: " <> puzzle

readInput :: [String] -> IO String
readInput args = do
  case args of
    [] -> error "Input file name expected."
    (file:_) -> readFile file

fromInput :: String -> [[Int]]
fromInput = map (map read . words) . lines

showResult :: Int -> IO ()
showResult result = putStrLn $ "Result: " <> show result

--------------------------------------------------------------------------------
-- Day 1
--------------------------------------------------------------------------------

puzzle1A :: [String] -> IO ()
puzzle1A args = do
  input <- readInput args
  case L.transpose $ fromInput input of
    [list1, list2] -> do
      let sortedList1 = L.sort list1
          sortedList2 = L.sort list2
          result = sum $ zipWith (\i1 i2 -> abs (i1 - i2)) sortedList1 sortedList2
      showResult result
    _ -> putStrLn "Input file not in expected format."

puzzle1B :: [String] -> IO ()
puzzle1B args = do
  input <- readInput args
  let input' :: [[Int]]
      input' = L.transpose $ fromInput input
  case input' of
    [list1, list2] -> do
      let count n = length $ filter (== n) list2
          result = sum $ map (\i -> i * count i) list1
      showResult result
    _ -> putStrLn "Input file not in expected format."

--------------------------------------------------------------------------------
-- Day 2
--------------------------------------------------------------------------------

puzzle2A :: [String] -> IO ()
puzzle2A args = do
  input <- readInput args
  let reports = fromInput input
      result = length $ filter isSafe reports
  showResult result
  where

isSafe :: [Int] -> Bool
isSafe xs = isSafe' xs || isSafe' (reverse xs)
 where
  isSafe' [] = True
  isSafe' [_] = True
  isSafe' (x1:x2:xs') = d <= 3 && d >= 1 && isSafe' (x2:xs')
   where
    d = x1 - x2

puzzle2B :: [String] -> IO ()
puzzle2B args = do
  input <- readInput args
  let reports = fromInput input
      result = length $ filter isSafe' reports
  showResult result
  where
   isSafe' xs = any isSafe (xs : dropped xs)
   dropped xs = map (remainder xs) [1 .. length xs]
   remainder xs n = [i | (i, m) <- zip xs [1 ..], m /= n]

--------------------------------------------------------------------------------
-- Day 3
--------------------------------------------------------------------------------

puzzle3A :: [String] -> IO ()
puzzle3A args = do
  input <- readInput args
  let result = puzzle3A' input
  showResult result

puzzle3A' :: String -> Int
puzzle3A' input =
  let parseResult = Parsec.parse muls "" input
      rawResult = case parseResult of
        Left _ -> error "Parsing failed."
        Right result -> result
  in  sum $ map eval rawResult

eval :: Instruction -> Int
eval Enable = 0
eval Disable = 0
eval (Mul x y) = if x < 1000 && y < 1000 then x * y else 0

muls :: Parsec.Parsec String () [Instruction]
muls = Parsec.many (Parsec.try (garbage mul >> Parsec.try mul))

mul :: Parsec.Parsec String () Instruction
mul = do
  void $ Parsec.string "mul("
  x <- Parsec.many1 Parsec.digit
  void $ Parsec.char ','
  y <- Parsec.many1 Parsec.digit
  void $ Parsec.char ')'
  pure $ Mul (read x) (read y)

garbage :: Parsec.Parsec String () Instruction -> Parsec.Parsec String () String
garbage p = Parsec.manyTill Parsec.anyChar (Parsec.lookAhead (Parsec.try p))

puzzle3B :: [String] -> IO ()
puzzle3B args = do
  input <- readInput args
  let result = puzzle3B' input
  showResult result

puzzle3B' :: String -> Int
puzzle3B' input =
  let parseResult = Parsec.parse instructions "" input
      rawResult = case parseResult of
        Left _ -> error "Parsing failed."
        Right result -> result
      sumWithState :: (MulState, Int) -> Instruction -> (MulState, Int)
      sumWithState (mulState, acc) i = case i of
        Enable-> (Enabled, acc)
        Disable -> (Disabled, acc)
        Mul _ _ -> (mulState, acc + evalWithState mulState i)
  in  snd $ L.foldl' sumWithState (Enabled, 0) rawResult

data Instruction = Enable | Disable | Mul !Int !Int deriving Show

data MulState = Enabled | Disabled

instruction :: Parsec.Parsec String () Instruction
instruction = Parsec.try enable Parsec.<|> Parsec.try disable Parsec.<|> Parsec.try mul

instructions :: Parsec.Parsec String () [Instruction]
instructions = Parsec.many (Parsec.try (garbage instruction >> Parsec.try instruction))

enable :: Parsec.Parsec String () Instruction
enable = do
  void $ Parsec.string "do()"
  pure Enable

disable :: Parsec.Parsec String () Instruction
disable = do
  void $ Parsec.string "don't()"
  pure Disable

evalWithState :: MulState -> Instruction -> Int
evalWithState Disabled _ = 0
evalWithState Enabled i = eval i
