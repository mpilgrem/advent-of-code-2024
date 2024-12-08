module Main
  ( main
  ) where

import qualified Data.List as L
import           System.Environment ( getArgs )

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
      _ -> putStrLn $ "Unknown puzzle: " <> puzzle

readInput :: [String] -> IO String
readInput args = do
  case args of
    [] -> error "Input file name expected."
    (file:_) -> readFile file

fromInput :: String -> [[Int]]
fromInput = (map (map read . words)) . lines

showResult :: Int -> IO ()
showResult result = putStrLn $ "Result: " <> show result

puzzle1A :: [String] -> IO ()
puzzle1A args = do
  input <- readInput args
  case L.transpose $ fromInput input of
    (list1:list2:[]) -> do
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
    (list1:list2:[]) -> do
      let count n = length $ filter (== n) list2
          result = sum $ map (\i -> i * count i) list1
      showResult result
    _ -> putStrLn "Input file not in expected format."

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
