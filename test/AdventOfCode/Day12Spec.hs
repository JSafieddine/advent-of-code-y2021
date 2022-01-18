module AdventOfCode.Day12Spec (spec) where

import Data.Functor ((<&>))

import Test.Hspec

import AdventOfCode.Day12


spec :: Spec
spec = do
  describe "test day 12" $ do
    it "part one" $
      (readFile "test/Data/Day12.txt" <&> solverPartOne) `shouldReturn` 226
    it "part two" $
      (readFile "test/Data/Day12.txt" <&> solverPartTwo) `shouldReturn` 3509
