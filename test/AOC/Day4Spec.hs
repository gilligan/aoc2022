module AOC.Day4Spec where

import AOC.Day4
import AOC.Util
import Test.Hspec

sampleInput :: String
sampleInput =
  unlines
    [ "2-4,6-8",
      "2-3,4-5",
      "5-7,7-9",
      "2-8,3-7",
      "6-6,4-6",
      "2-6,4-8"
    ]

spec :: Spec
spec = do
  describe "Day4" $ do
    describe "Assignment" $ do
      it "can be parsed from string" $ do
        case parseString parseAssignment mempty "1-2" of
          Success a -> a `shouldBe` Assignment 1 2
          Failure f -> expectationFailure (show f)
      it "can be parsed as a pair" $ do
        case parseString parseAssignmentPair mempty "1-2,3-4" of
          Success a -> a `shouldBe` (Assignment 1 2, Assignment 3 4)
          Failure f -> expectationFailure (show f)
      it "can contain another Assignment" $ do
        (Assignment 1 6 `contains` Assignment 3 3) `shouldBe` True
        (Assignment 1 6 `contains` Assignment 3 9) `shouldBe` False
    describe "part1" $ do
      it "yields 2 for the example" $ do
        part1 sampleInput `shouldBe` 2
      it "yields 0 for my puzzle input" $ do
        input <- readFile "./data/day4.txt"
        part1 input `shouldBe` 538
