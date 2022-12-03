module AOC.Day1 (findMax, findMaxTop3) where

import qualified Data.List as L
import qualified Data.Text as T

findMax :: T.Text -> Integer
findMax t = uncurry max $ foldr go (0, 0) (T.lines t)
  where
    go x (max, curr)
      | x == "" = if curr > max then (curr, 0) else (max, 0)
      | otherwise = (max, curr + read (T.unpack x))

findMaxTop3 :: T.Text -> Integer
findMaxTop3 t = get $ foldr go ([], 0) (T.lines t)
  where
    go x (m, curr)
      | x == "" = (curr : m, 0)
      | otherwise = (m, curr + read (T.unpack x) :: Integer)
    get (max, curr) = sum . take 3 . L.sortBy (flip compare) $ curr : max
