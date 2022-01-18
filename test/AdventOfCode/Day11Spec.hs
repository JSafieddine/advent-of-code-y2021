module AdventOfCode.Day11Spec (spec) where

import Data.Functor ((<&>))

import Test.Hspec

import AdventOfCode.Day11


spec :: Spec
spec = do
  describe "test day 11" $ do
    it "part one" $
      (readFile "test/Data/Day11.txt" <&> solverPartOne) `shouldReturn` 1656
    it "part two" $
      (readFile "test/Data/Day11.txt" <&> solverPartTwo) `shouldReturn` 195
