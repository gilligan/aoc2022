module AOC.Day2 where

import AOC.Util
import Control.Applicative
import Data.Functor

data RPS = Rock | Paper | Scissors
  deriving (Eq, Show)

data Strategy = Lose | Draw | Win
  deriving (Eq, Show)

parseElfRPS :: Parser RPS
parseElfRPS =
  char 'A' $> Rock
    <|> char 'B' $> Paper
    <|> char 'C' $> Scissors

parseMyRPS :: Parser RPS
parseMyRPS =
  char 'X' $> Rock
    <|> char 'Y' $> Paper
    <|> char 'Z' $> Scissors

parseStrategy :: Parser Strategy
parseStrategy =
  char 'X' $> Lose
    <|> char 'Y' $> Draw
    <|> char 'Z' $> Win

parseRPS :: Parser RPS
parseRPS = parseElfRPS <|> parseMyRPS

parseStrategicRPS :: Parser (RPS, Strategy)
parseStrategicRPS = do
  x <- parseElfRPS
  _ <- char ' '
  s <- parseStrategy
  return (x, s)

-- rock = 1
-- paper = 2
-- scissors = 3
-- lost = 0
-- draw = 3
-- won = 6

scoreRound :: RPS -> RPS -> Integer
scoreRound Rock Rock = 1 + 3
scoreRound Rock Paper = 2 + 6
scoreRound Rock Scissors = 3 + 0
scoreRound Paper Rock = 1 + 0
scoreRound Paper Paper = 2 + 3
scoreRound Paper Scissors = 3 + 6
scoreRound Scissors Rock = 1 + 6
scoreRound Scissors Paper = 2 + 0
scoreRound Scissors Scissors = 3 + 3

scoreStrategicRound :: (RPS, Strategy) -> Integer
scoreStrategicRound (Rock, Lose) = scoreRound Rock Scissors
scoreStrategicRound (Rock, Draw) = scoreRound Rock Rock
scoreStrategicRound (Rock, Win) = scoreRound Rock Paper
scoreStrategicRound (Paper, Lose) = scoreRound Paper Rock
scoreStrategicRound (Paper, Draw) = scoreRound Paper Paper
scoreStrategicRound (Paper, Win) = scoreRound Paper Scissors
scoreStrategicRound (Scissors, Lose) = scoreRound Scissors Paper
scoreStrategicRound (Scissors, Draw) = scoreRound Scissors Scissors
scoreStrategicRound (Scissors, Win) = scoreRound Scissors Rock

getScore :: String -> Integer
getScore t = case parsed of
  Success x -> sum $ fmap calc x
  Failure _ -> error "Failed parsing input"
  where
    rounds = words <$> lines t
    parse = parseString parseRPS mempty
    parsed = traverse (traverse parse) rounds
    calc xs = scoreRound (head xs) (xs !! 1)

getStrategicScore :: String -> Integer
getStrategicScore t = case parsed of
  Success x -> sum $ fmap scoreStrategicRound x
  Failure _ -> error "Failed parsing input"
  where
    rounds = lines t
    parse = parseString parseStrategicRPS mempty
    parsed = traverse parse rounds
