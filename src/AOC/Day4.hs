module AOC.Day4 where

import AOC.Util

sample :: String
sample =
  unlines
    [ "2-4,6-8",
      "2-3,4-5",
      "5-7,7-9",
      "2-8,3-7",
      "6-6,4-6",
      "2-6,4-8"
    ]

data Assignment = Assignment
  { from :: Integer,
    to :: Integer
  }
  deriving (Eq, Show)

parseAssignment :: Parser Assignment
parseAssignment = do
  from <- integer
  _ <- char '-'
  Assignment from <$> integer

parseAssignmentPair :: Parser (Assignment, Assignment)
parseAssignmentPair = do
  a <- parseAssignment
  _ <- char ','
  b <- parseAssignment
  return (a, b)

contains :: Assignment -> Assignment -> Bool
contains a b
  | from a <= from b && to a >= to b = True
  | otherwise = False

part1 :: String -> Int
part1 s = case sequence (getPairs s) of
  Success as -> (length . filter id) $ fmap eitherContains as
  Failure _ -> error "failed to parse input"
  where
    getPairs s = parseString parseAssignmentPair mempty <$> lines s
    eitherContains (a, b) = a `contains` b || b `contains` a

part2 :: String -> Int
part2 s = case sequence (getPairs s) of
  Success as -> (length . filter id) $ fmap (uncurry overlap) as
  Failure _ -> error "failed to parse input"
  where
    getPairs s = parseString parseAssignmentPair mempty <$> lines s
    overlap (Assignment x1 y1) (Assignment x2 y2) = x1 <= y2 && x2 <= y1
