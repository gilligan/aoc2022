module AOC.Day2Spec where

import AOC.Day2
import AOC.Util
import Test.Hspec

rpsSample :: String
rpsSample =
  unlines
    [ "A Y",
      "B X",
      "C Z"
    ]

spec :: Spec
spec = do
  describe "Day2" $ do
    describe "parseRPS" $ do
      it "parses all letters correctly" $ do
        case traverse (parseString parseRPS mempty) ["A", "B", "C", "X", "Y", "Z"] of
          Success r -> r `shouldBe` [Rock, Paper, Scissors, Rock, Paper, Scissors]
          Failure _ -> expectationFailure "should have parsed correctly"
    describe "getScore" $ do
      it "should solve the sample correctly" $ do
        getScore rpsSample `shouldBe` 15
      it "should solve my puzzle input correctly" $ do
        input <- readFile "./data/day2.txt"
        getScore input `shouldBe` 15691
    describe "parseStrategicRPS" $ do
      it "should parse all letters correctly" $ do
        case traverse (parseString parseStrategicRPS mempty) (lines rpsSample) of
          Success r -> r `shouldBe` [(Rock, Draw), (Paper, Lose), (Scissors, Win)]
          Failure _ -> expectationFailure "should have parsed correctly"
    describe "getStrategicScore" $ do
      it "should solve the sample correctly" $ do
        getStrategicScore rpsSample `shouldBe` 12
      it "should parse my puzzle input correctly" $ do
        input <- readFile "./data/day2.txt"
        getStrategicScore input `shouldBe` 12989
