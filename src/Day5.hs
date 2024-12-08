module Day5 where

import           Data.Functor ( void )
import qualified Text.Parsec as Parsec

import           Utils

puzzle5A :: [String] -> IO ()
puzzle5A args = do
  input <- readInput args
  let (rules, rest) = span (/= "") (lines input)
      result = puzzle5A' rules (filter (/= "") rest)
  showResult result

puzzle5A' :: [String] -> [String] -> Int
puzzle5A' rawRules rawUpdates =
  let rules = map toRule rawRules
      updates = map toUpdate rawUpdates
      validUpdates = filter (validUpdate rules) updates
  in  sum $ map midPage validUpdates

data Rule = Order !Int !Int

midPage :: [Int] -> Int
midPage updates =
  let l = length updates
  in  if even l
        then error "Update has no middle page!"
        else updates !! (l `div` 2)

toRule :: String -> Rule
toRule rawRule = case Parsec.parse rule "" rawRule of
  Right r -> r
  Left _ -> error rawRule

rule :: Parsec.Parsec String () Rule
rule = do
  p1 <- decimal
  void $ Parsec.char '|'
  Order p1 <$> decimal

toUpdate :: String -> [Int]
toUpdate s = case Parsec.parse update "" s of
  Right u -> u
  Left _ -> error s

update :: Parsec.Parsec String () [Int]
update = Parsec.sepBy decimal (Parsec.char ',')

decimal :: Parsec.Parsec String () Int
decimal = read <$> Parsec.many1 Parsec.digit

toPairs :: [Int] -> [(Int, Int)]
toPairs xs =
  let n = length xs
  in  [(xs !! i, xs !! j) | i <- [0 .. n - 2], j <- [i + 1 .. n - 1]]

validUpdate :: [Rule] -> [Int] -> Bool
validUpdate rules update =
  let pairs = toPairs update
  in  all (validPair rules) pairs

validPair :: [Rule] -> (Int, Int) -> Bool
validPair rules pair =
  any (followRule pair) rules && not (any (breakRule pair) rules)

followRule :: (Int, Int) -> Rule -> Bool
followRule (p1, p2) (Order p1' p2') = p1 == p1' && p2 == p2'

breakRule ::  (Int, Int) -> Rule -> Bool
breakRule (p1, p2) (Order p1' p2') = p1 == p2' && p2 == p1'
