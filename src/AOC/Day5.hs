{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module AOC.Day5 where

import AOC.Util
import qualified Data.List as L
import qualified Data.Text as T

data Instruction = Move
  { count :: Integer,
    from :: Integer,
    to :: Integer
  }
  deriving (Eq, Show)

type Stacks = [[String]]

testStack :: String
testStack =
  unlines
    [ "    [D]    ",
      "[N] [C]    ",
      "[Z] [M] [P]"
    ]

parseInst :: Parser Instruction
parseInst = do
  count <- symbol "move" *> integer
  from <- symbol "from" *> integer
  _ <- symbol "to "
  Move count from <$> integer

stripWhitespace :: String -> String
stripWhitespace = T.unpack . T.strip . T.pack

getStacks :: String -> [[String]]
getStacks str = filter (/= "") <$> L.transpose (fmap stripWhitespace <$> chunks)
  where
    chunks = chunksOf 4 <$> lines str

exec :: Instruction -> Stacks -> Stacks
exec (Move c f t) s =
  let removed = reverse $ take (fromIntegral c) $ s !! (fromIntegral f - 1)
      s' = update (f - 1) (drop (fromInteger c)) s
   in update (t - 1) (removed ++) s'

execProgram :: [Instruction] -> Stacks -> Stacks
execProgram is s = foldl (flip exec) s is

exec' :: Instruction -> Stacks -> Stacks
exec' (Move c f t) s =
  let removed = take (fromIntegral c) $ s !! (fromIntegral f - 1)
      s' = update (f - 1) (drop (fromInteger c)) s
   in update (t - 1) (removed ++) s'

execProgram' :: [Instruction] -> Stacks -> Stacks
execProgram' is s = foldl (flip exec') s is

parseInput :: String -> Maybe (Stacks, [Instruction])
parseInput str =
  let ls = lines str
      (Just n) = L.elemIndex "" ls
      stacks = getStacks (unlines $ take (n - 1) ls)
      program = traverse (parseString parseInst mempty) (drop (n + 1) ls)
   in case program of
        Success p -> Just (stacks, p)
        Failure x -> error (show x)

part1 :: String -> String
part1 str =
  let input = parseInput str
   in case input of
        Just (stacks, insts) -> concatMap head (execProgram insts stacks)
        Nothing -> "error parsing input"

part2 :: String -> String
part2 str =
  let input = parseInput str
   in case input of
        Just (stacks, insts) -> concatMap head (execProgram' insts stacks)
        Nothing -> "error parsing input"
