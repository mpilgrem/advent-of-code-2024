module Day1 where

import qualified Data.List as L

import           Utils

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
