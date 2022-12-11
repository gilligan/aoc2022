module AOC.Day6 where

import qualified Data.List as L

lists :: Int -> String -> [String]
lists _ [] = []
lists n l@(_ : xs) = take n l : lists n xs

uniq :: Eq a => [a] -> Bool
uniq s = length s == (length . L.nub $ s)

findMarkerPos :: Int -> String -> Int
findMarkerPos n s = (n +) $ length $ takeWhile (not . uniq) $ lists n s

part1 :: String -> Int
part1 = findMarkerPos 4

part2 :: String -> Int
part2 = findMarkerPos 14
