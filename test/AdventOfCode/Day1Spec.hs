module AdventOfCode.Day1Spec (spec) where

import Test.Hspec
import AdventOfCode.Day1


spec :: Spec
spec = do
  describe "test day 1" $ do
    it "part one" $
      (readFile "test/Data/Day1.txt" >>= return . solverPartOne) `shouldReturn` 7
    it "part two" $
      (readFile "test/Data/Day1.txt" >>= return . solverPartTwo) `shouldReturn` 5