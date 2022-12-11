module AOC.Day5Spec where

import AOC.Day5
import AOC.Util
import Test.Hspec

testInput :: String
testInput =
  unlines
    [ "    [D]    ",
      "[N] [C]    ",
      "[Z] [M] [P]",
      " 1   2   3 ",
      "",
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
    ]

sample :: String
sample =
  unlines
    [ "    [D]    ",
      "[N] [C]    ",
      "[Z] [M] [P]"
    ]

spec :: Spec
spec = do
  describe "Day5" $ do
    describe "getStacks" $ do
      it "turns the ascii into a list of lists" $ do
        getStacks sample `shouldBe` [["[N]", "[Z]"], ["[D]", "[C]", "[M]"], ["[P]"]]
    describe "parseInst" $ do
      it "parses a move instruction" $ do
        case parseString parseInst mempty "move 1 from 1 to 3" of
          Success x -> x `shouldBe` Move 1 1 3
          Failure f -> expectationFailure (show f)
    describe "execProgram" $ do
      it "runs the program correctly on the sample data" $ do
        let program =
              [ Move 1 2 1,
                Move 3 1 3,
                Move 2 2 1,
                Move 1 1 2
              ]
         in execProgram program (getStacks sample) `shouldBe` [["[C]"], ["[M]"], ["[Z]", "[N]", "[D]", "[P]"]]
    describe "parseInput" $ do
      it "parses the sample input correctly" $ do
        case parseInput testInput of
          Just x -> x `shouldBe` ([["[N]", "[Z]"], ["[D]", "[C]", "[M]"], ["[P]"]], [Move 1 2 1, Move 3 1 3, Move 2 2 1, Move 1 1 2])
          Nothing -> expectationFailure "failed to parse"
    describe "part1" $ do
      it "yields the correct output for the sample input" $ do
        part1 testInput `shouldBe` "[C][M][Z]"
      it "yields the correct output for my puzzle input" $ do
        fileInput <- readFile "./data/day5.txt"
        part1 fileInput `shouldBe` "[F][Z][C][M][J][C][R][H][Z]"
    describe "part2" $ do
      it "yields the correct output for the sample input" $ do
        part2 testInput `shouldBe` "[M][C][D]"
      it "yields the correct output for my puzzle input" $ do
        fileInput <- readFile "./data/day5.txt"
        part2 fileInput `shouldBe` "[J][S][D][H][Q][M][Z][G][F]"
