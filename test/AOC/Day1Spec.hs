module AOC.Day1Spec (spec) where

import AOC.Day1
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Test.Hspec

caloriesSample :: T.Text
caloriesSample =
  T.unlines
    [ "1000",
      "2000",
      "3000",
      "",
      "4000",
      "",
      "5000",
      "6000",
      "",
      "7000",
      "8000",
      "9000",
      "",
      "10000"
    ]

spec :: Spec
spec = do
  describe "Day1" $ do
    describe "findMax" $ do
      it "should find the maximum for test input" $ do
        findMax caloriesSample `shouldBe` 24000
      it "should find the maximum for my puzzle input" $ do
        input <- TIO.readFile "./data/day1.txt"
        findMax input `shouldBe` 70116
    describe "findMaxTop3" $ do
      it "should find the top3 maximum for the test input" $ do
        findMaxTop3 caloriesSample `shouldBe` 45000
      it "should find the top3 maximum for my puzzle input" $ do
        input <- TIO.readFile "./data/day1.txt"
        findMaxTop3 input `shouldBe` 206582
