module AdventOfCode.Day7Spec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Data.Functor ((<&>))

import Text.Megaparsec (runParser)


import AdventOfCode.Day7


spec :: Spec
spec = do
  describe "test day 7" $ do
    it "test inputParser" $ do
      input <- readFile "test/Data/Day7.txt"
      runParser inputParser "" input `shouldParse` [16,1,2,0,4,2,7,1,2,14]
    it "part one" $
      (readFile "test/Data/Day7.txt" <&> solverPartOne) `shouldReturn` 37
    it "part two" $
      (readFile "test/Data/Day7.txt" <&> solverPartTwo) `shouldReturn` 168
