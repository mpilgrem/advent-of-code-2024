module Main
  ( main
  ) where

import           System.Environment ( getArgs )

import           Day1 ( puzzle1A, puzzle1B )
import           Day2 ( puzzle2A, puzzle2B )
import           Day3 ( puzzle3A, puzzle3B )
import           Day5 ( puzzle5A )

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
      "5A" -> puzzle5A args'
      _ -> putStrLn $ "Unknown puzzle: " <> puzzle
