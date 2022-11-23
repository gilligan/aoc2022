module AOC.Day1Spec (spec) where

import AOC.Day1
import Test.Hspec

spec :: Spec
spec = do
  describe "Day1" $ do
    describe "dummy" $ do
      it "foo shouldbe foo" $ do
        foo `shouldBe` "foo"
