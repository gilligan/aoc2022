module AOC.Day7Spec where

import AOC.Day7
import AOC.Util
import Test.Hspec

testTerminalOutput :: String
testTerminalOutput =
  unlines
    [ "$ cd /",
      "$ ls",
      "dir a",
      "14848514 b.txt",
      "8504156 c.dat",
      "dir d",
      "$ cd a",
      "$ ls",
      "dir e",
      "29116 f",
      "2557 g",
      "62596 h.lst",
      "$ cd e",
      "$ ls",
      "584 i",
      "$ cd ..",
      "$ cd ..",
      "$ cd d",
      "$ ls",
      "4060174 j",
      "8033020 d.log",
      "5626152 d.ext",
      "7214296 k"
    ]

spec :: Spec
spec = do
  describe "Day 7" $ do
    describe "mkdir" $ do
      it "adds a dir to root" $ do
        let fs = Dir "/" []
        mkdir ["/", "etc"] fs `shouldBe` Dir "/" [Dir "etc" []]
      it "adds dir to nested directory" $ do
        let fs = Dir "/" [Dir "foo" []]
        mkdir ["/", "foo", "bar"] fs `shouldBe` Dir "/" [Dir "foo" [Dir "bar" []]]
      it "adds dir to nested directory with dirs on the same level" $ do
        let fs = Dir "/" [Dir "foo" [], Dir "1" [], Dir "2" []]
        mkdir ["/", "foo", "bar"] fs `shouldBe` Dir "/" [Dir "foo" [Dir "bar" []], Dir "1" [], Dir "2" []]
      it "adds a dir to root next to existing directory" $ do
        let fs = Dir "/" [Dir "a" []]
        mkdir ["/", "b"] fs `shouldBe` Dir "/" [Dir "a" [], Dir "b" []]
    describe "touch" $ do
      it "adds a file to root" $ do
        let fs = Dir "/" []
        touch (["/", "foo"], 100) fs `shouldBe` Dir "/" [File "foo" 100]
      it "adds a file to non-empty root" $ do
        let fs = Dir "/" [Dir "a" []]
        touch (["/", "b.text"], 100) fs `shouldBe` Dir "/" [Dir "a" [], File "b.text" 100]
      it "adds a file to a nested directory" $ do
        let fs = Dir "/" [Dir "foo" []]
        touch (["/", "foo", "bar"], 100) fs `shouldBe` Dir "/" [Dir "foo" [File "bar" 100]]
      it "adds file to nested directory with dirs on the same level" $ do
        let fs = Dir "/" [Dir "foo" [], Dir "1" [], Dir "2" []]
        touch (["/", "foo", "bar.txt"], 100) fs `shouldBe` Dir "/" [Dir "foo" [File "bar.txt" 100], Dir "1" [], Dir "2" []]
      it "adds a file to root next to existing directory" $ do
        let fs = Dir "/" [Dir "a" []]
        touch (["/", "b.text"], 100) fs `shouldBe` Dir "/" [Dir "a" [], File "b.text" 100]
    describe "parseTermOutput" $ do
      it "should parse the given sample" $ do
        let parsed = traverse (parseString parseTermOutput mempty) (lines sampleTerminal)
        case parsed of
          Success res ->
            res
              `shouldBe` [ TermCd "/",
                           TermLs,
                           TermDir "a",
                           TermFile 14848514 "b.txt",
                           TermFile 8504156 "c.dat",
                           TermDir "d",
                           TermCd "a",
                           TermLs,
                           TermDir "e",
                           TermFile 29116 "f",
                           TermFile 2557 "g",
                           TermFile 62596 "h.lst",
                           TermCd "e",
                           TermLs,
                           TermFile 584 "i",
                           TermUp,
                           TermUp,
                           TermCd "d",
                           TermLs,
                           TermFile 4060174 "j",
                           TermFile 8033020 "d.log",
                           TermFile 5626152 "d.ext",
                           TermFile 7214296 "k"
                         ]
          Failure x -> expectationFailure (show x)
    describe "processTerm" $ do
      it "should create initial root" $ do
        processTerm [TermCd "/"] `shouldBe` Dir "/" []
      it "should not do anything for ls on its own" $ do
        processTerm [TermCd "/", TermLs] `shouldBe` Dir "/" []
      it "should create a directory when listed by `ls`" $ do
        processTerm [TermCd "/", TermLs, TermDir "etc"] `shouldBe` Dir "/" [Dir "etc" []]
      it "should create a file when listed by `ls`" $ do
        processTerm [TermCd "/", TermLs, TermFile 100 "root.txt"] `shouldBe` Dir "/" [File "root.txt" 100]
    describe "part1" $ do
      it "should solve the example output correctly" $ do
        part1 testTerminalOutput `shouldBe` 95437
      it "should solve my puzzle input correctly" $ do
        puzzleInput <- readFile "./data/day7.txt"
        part1 puzzleInput `shouldBe` 1513699
    describe "part 2" $ do
      it "should solve the example correctly" $ do
        part2 testTerminalOutput `shouldBe` 24933642
      it "should solve my puzzle input correctly" $ do
        puzzleInput <- readFile "./data/day7.txt"
        part2 puzzleInput `shouldBe` 7991939
