module AdventOfCode.Day15Spec (spec) where

import Data.Functor ((<&>))

import Test.Hspec

import AdventOfCode.Day15


spec :: Spec
spec = do
  describe "test day 15" $ do
    it "part one" $
      (readFile "test/Data/Day15.txt" <&> solverPartOne) `shouldReturn` 40
    it "part two" $
      (readFile "test/Data/Day15.txt" <&> solverPartTwo) `shouldReturn` -2

