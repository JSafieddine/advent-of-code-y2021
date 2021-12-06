module AdventOfCode.Day6Spec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Data.Functor ((<&>))

import Text.Megaparsec (runParser)


import AdventOfCode.Day6


spec :: Spec
spec = do
  describe "test day 1" $ do
    it "test inputParser" $ do
      input <- readFile "test/Data/Day6.txt"
      runParser inputParser "" input `shouldParse` [3,4,3,1,2]
    it "part one" $
      (readFile "test/Data/Day6.txt" <&> solverPartOne) `shouldReturn` 5934
    it "part two" $
      (readFile "test/Data/Day6.txt" <&> solverPartTwo) `shouldReturn` 26984457539
