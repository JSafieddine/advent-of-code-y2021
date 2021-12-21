module AdventOfCode.Day19Spec (spec) where

import Data.Functor ((<&>))

import Test.Hspec

import AdventOfCode.Day19


spec :: Spec
spec = do
  describe "test day 19" $ do
    it "part one" $
      (readFile "test/Data/Day19.txt" <&> solverPartOne) `shouldReturn` 79
    it "part two" $
      (readFile "test/Data/Day19.txt" <&> solverPartTwo) `shouldReturn` -2

