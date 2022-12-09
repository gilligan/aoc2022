module AOC.Day3 where

import Data.Bits (shiftR)
import Data.Char (isAsciiLower, ord)

--
-- part 1
--

type Rucksack = (String, String)

findDup :: Rucksack -> Maybe Char
findDup (l, r) =
  case filter (`elem` r) l of
    [] -> Nothing
    xs -> Just (head xs)

prio :: Char -> Int
prio c
  | isAsciiLower c = ord c - (ord 'a' - 1)
  | otherwise = ord c - (ord 'A' - 27)

getPrioSum :: String -> Maybe Int
getPrioSum s = fmap sum . getPrios <$> lines $ s
  where
    getPrios = traverse (fmap prio . findDup . split)
    split s = splitAt (length s `shiftR` 1) s

--
-- part 2
--

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0 = error "invalid split size"
  | null xs = []
  | otherwise = take n xs : chunksOf n (drop n xs)

findTripleDup :: [String] -> Maybe Char
findTripleDup xs =
  case filter (\x -> x `elem` (xs !! 1) && x `elem` (xs !! 2)) (head xs) of
    [] -> Nothing
    xs -> Just (head xs)

getPrioSum' :: String -> Maybe Int
getPrioSum' s = sum <$> traverse (fmap prio . findTripleDup) groups
  where
    groups = chunksOf 3 (lines s)
