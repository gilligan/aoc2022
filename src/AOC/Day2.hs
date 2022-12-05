module AOC.Day2 where

import AOC.Util
import Control.Applicative
import Data.Functor

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

parseElfRPS :: Parser RPS
parseElfRPS =
  symbolic 'A' $> Rock
    <|> symbolic 'B' $> Paper
    <|> symbolic 'C' $> Scissors

parseMyRPS :: Parser RPS
parseMyRPS =
  symbolic 'X' $> Rock
    <|> symbolic 'Y' $> Paper
    <|> symbolic 'Z' $> Scissors

parseRPS :: Parser RPS
parseRPS = parseElfRPS <|> parseMyRPS

-- rock = 1
-- paper = 2
-- scissors = 3
-- lost = 0
-- draw = 3
-- won = 6

score :: RPS -> RPS -> Integer
score Rock Rock = 1 + 3
score Rock Paper = 2 + 6
score Rock Scissors = 3 + 0
score Paper Rock = 1 + 0
score Paper Paper = 2 + 3
score Paper Scissors = 3 + 6
score Scissors Rock = 1 + 6
score Scissors Paper = 2 + 0
score Scissors Scissors = 3 + 3

getScore :: String -> Integer
getScore t = case parsed of
  Success x -> sum $ fmap calc x
  Failure _ -> error "Failed parsing input"
  where
    rounds = words <$> lines t
    parse = parseString parseRPS mempty
    parsed = traverse (traverse parse) rounds
    calc xs = score (head xs) (xs !! 1)
