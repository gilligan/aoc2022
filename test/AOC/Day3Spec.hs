module AOC.Day3Spec where

import AOC.Day3
import Test.Hspec

sampleRucksacks :: String
sampleRucksacks =
  unlines
    [ "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg",
      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]

spec :: Spec
spec = do
  describe "Day3" $ do
    describe "findDup" $ do
      it "should find 'p' for the example given" $ do
        findDup (splitAt 12 "vJrwpWtwJgWrhcsFMMfFFhFp") `shouldBe` Just 'p'
      it "should find 'L' for the example given" $ do
        findDup (splitAt 16 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL") `shouldBe` Just 'L'
      it "should find P for the example given" $ do
        findDup (splitAt 9 "PmmdzqPrVvPwwTWBwg") `shouldBe` Just 'P'
      it "should find v for the example given" $ do
        findDup (splitAt 15 "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn") `shouldBe` Just 'v'
    describe "getPrioSum" $ do
      it "should find the correct prio for the example given" $ do
        getPrioSum sampleRucksacks `shouldBe` Just 157
      it "should yield the right result for my puzzle input" $ do
        input <- readFile "./data/day3.txt"
        getPrioSum input `shouldBe` Just 8493
    describe "getPrioSum'" $ do
      it "should find the correct prio for the example given" $ do
        getPrioSum' sampleRucksacks `shouldBe` Just 70
      it "should yield the right result for my puzzle input" $ do
        input <- readFile "./data/day3.txt"
        getPrioSum' input `shouldBe` Just 2552
