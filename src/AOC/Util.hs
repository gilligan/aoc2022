module AOC.Util
  ( module Text.Trifecta,
    readItemsFromFile,
    readItemsFromFileWith,
    parseFile,
  )
where

import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Trifecta

parseFile :: FilePath -> Parser a -> IO (Result a)
parseFile f p = TIO.readFile f <&> (parseString p mempty . T.unpack)

readItemsFromFile :: (Read a) => FilePath -> IO [a]
readItemsFromFile p = readItemsFromFileWith p (read . T.unpack)

readItemsFromFileWith :: FilePath -> (T.Text -> a) -> IO [a]
readItemsFromFileWith p f = do
  contents <- TIO.readFile p
  let lines = init $ T.splitOn "\n" contents
  return $ f <$> lines
