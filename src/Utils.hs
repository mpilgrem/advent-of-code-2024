module Utils
  ( readInput
  , fromInput
  , showResult
  ) where

readInput :: [String] -> IO String
readInput args = do
  case args of
    [] -> error "Input file name expected."
    (file:_) -> readFile file

fromInput :: String -> [[Int]]
fromInput = map (map read . words) . lines

showResult :: Int -> IO ()
showResult result = putStrLn $ "Result: " <> show result
