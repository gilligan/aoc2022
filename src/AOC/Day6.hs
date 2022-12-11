module AOC.Day6 where

import qualified Data.List as L

lists :: String -> [String]
lists [] = []
lists l@(x : xs) = take 4 l : lists xs

uniq :: Eq a => [a] -> Bool
uniq s = length s == (length . L.nub $ s)

findMarkerPos :: String -> Int
findMarkerPos s = (4 +) $ length $ takeWhile (not . uniq) $ lists s

part1 :: String -> Int
part1 = findMarkerPos
