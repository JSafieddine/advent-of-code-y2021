module AdventOfCode.Day10Spec (spec) where

import Data.Functor ((<&>))

import Test.Hspec

import AdventOfCode.Day10


spec :: Spec
spec = do
  describe "test day 10" $ do
    it "part one" $
      (readFile "test/Data/Day10.txt" <&> solverPartOne) `shouldReturn` 26397
    it "part two" $
      (readFile "test/Data/Day10.txt" <&> solverPartTwo) `shouldReturn` 288957
