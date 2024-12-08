module Day3 where

import qualified Data.List as L
import           Data.Functor ( void )
import qualified Text.Parsec as Parsec

import           Utils

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
