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

sampleTree :: FileTree
sampleTree =
  Dir
    "/"
    [ Dir
        "a"
        [ Dir "e" [File "i" 584],
          File "f" 29116,
          File "g" 2557,
          File "h.lst" 62596
        ],
      File "b.txt" 14848514,
      File "c.dat" 8504156,
      Dir
        "d"
        [ File "j" 4060174,
          File "d.log" 8033020,
          File "d.ext" 5626152,
          File "k" 7214296
        ]
    ]

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

sampleTerminal :: String
sampleTerminal =
  unlines
    [ "$ cd /",
      "$ ls",
      "dir a",
      "14848514 b.txt",
      "8504156 c.dat",
      "dir d",
      "$ cd a",
      "$ ls",
      "dir e",
      "29116 f",
      "2557 g",
      "62596 h.lst",
      "$ cd e",
      "$ ls",
      "584 i",
      "$ cd ..",
      "$ cd ..",
      "$ cd d",
      "$ ls",
      "4060174 j",
      "8033020 d.log",
      "5626152 d.ext",
      "7214296 k"
    ]

getSum :: FileTree -> Int
getSum fs =
  let pickDirs (name, size) = name /= "/" && size <= 100000
      dirSizes = filter pickDirs $ getDirSizes fs
   in sum $ fmap snd dirSizes

part1 :: String -> Int
part1 str = case traverse (parseString parseTermOutput mempty) (lines str) of
  Success ops -> getSum $ processTerm ops
  Failure x -> error $ show x
    where
