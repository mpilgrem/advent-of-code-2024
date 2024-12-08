module Day2 where

import           Utils

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
