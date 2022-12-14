{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module AOC.Day7 where

import AOC.Util
import Control.Applicative
import Data.Functor

data FS a = File String a | Dir String [FS a]
  deriving (Eq, Show, Functor, Foldable, Traversable)

type FileTree = FS Int

type Path = [String]

data TermOutput
  = -- | "cd <dir>"
    TermCd String
  | -- | "cd .."
    TermUp
  | -- | - "ls"
    TermLs
  | -- | <size> <fileName>
    TermFile Integer String
  | -- | dir <fileName>
    TermDir String
  deriving (Show, Eq)

mkdir :: Path -> FileTree -> FileTree
mkdir _ (File n s) = File n s
mkdir [] fs = fs
mkdir (p : ps) d@(Dir name entries)
  | p == name && (length ps == 1) = Dir name (entries ++ [Dir (head ps) []])
  | p == name && (length ps > 1) = Dir name (mkdir ps <$> entries)
  | p /= name = d
mkdir _ x = x

touch :: (Path, Int) -> FileTree -> FileTree
touch _ (File n s) = File n s
touch ([], _) fs = fs
touch (p : ps, fileSize) d@(Dir name entries)
  | p == name && (length ps == 1) = Dir name (entries ++ [File (head ps) fileSize])
  | p == name && (length ps > 1) = Dir name (touch (ps, fileSize) <$> entries)
  | p /= name = d
touch _ x = x

getDirSizes :: FileTree -> [(String, Int)]
getDirSizes (Dir n entries) = (n, sum $ sum <$> entries) : concatMap getDirSizes entries
getDirSizes (File _ _) = []

parseTermOutput :: Parser TermOutput
parseTermOutput = try parseUp <|> try parseCd <|> try parseLs <|> parseFile <|> parseDir
  where
    parseUp = do
      _ <- symbol "$"
      _ <- symbol "cd"
      _ <- symbol ".."
      return TermUp
    parseCd = do
      _ <- symbol "$"
      _ <- symbol "cd"
      d <- some alphaNum <|> symbol "/"
      return $ TermCd d
    parseLs = symbol "$" *> symbol "ls" $> TermLs
    parseFile = do
      size <- integer
      whiteSpace
      name <- some (alphaNum <|> char '.')
      return $ TermFile size name
    parseDir = do
      _ <- symbol "dir"
      name <- some alphaNum
      return $ TermDir name

processTerm :: [TermOutput] -> FileTree
processTerm = go [] (Dir "/" [])
  where
    go :: Path -> FileTree -> [TermOutput] -> FileTree
    go [] fs (TermCd "/" : ops) = go ["/"] fs ops
    go path fs (op : ops) = case op of
      TermCd str -> go (path ++ [str]) fs ops
      TermUp -> go (init path) fs ops
      TermLs -> go path fs ops
      TermFile size name -> go path (touch (path ++ [name], fromIntegral size) fs) ops
      TermDir name -> go path (mkdir (path ++ [name]) fs) ops
    go _ fs [] = fs

getFileSum :: FileTree -> Int
getFileSum fs =
  let pickDirs (name, size) = name /= "/" && size <= 100000
      dirSizes = filter pickDirs $ getDirSizes fs
   in sum $ fmap snd dirSizes

findDirSizeToDelete :: [(String, Int)] -> Int
findDirSizeToDelete dirs =
  let spaceUsed = snd . head $ dirs
      spaceAvailable = 70000000 - spaceUsed
      spaceNeeded = 30000000 - spaceAvailable
   in minimum (filter (>= spaceNeeded) $ snd <$> dirs)

part1 :: String -> Int
part1 str = case traverse (parseString parseTermOutput mempty) (lines str) of
  Success ops -> getFileSum $ processTerm ops
  Failure x -> error $ show x

part2 :: String -> Int
part2 str = case traverse (parseString parseTermOutput mempty) (lines str) of
  Success ops -> findDirSizeToDelete $ getDirSizes $ processTerm ops
  Failure x -> error $ show x
