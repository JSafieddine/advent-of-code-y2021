module AdventOfCode.Day18Spec (spec) where

import Data.Functor ((<&>))

import Text.Megaparsec

import Test.Hspec
import Test.Hspec.Megaparsec

import AdventOfCode.Day18

spec :: Spec
spec = do
  describe "test day 18" $ do
    it "test snailFishNumberParser" $
        runParser snailFishNumberParser "" "[[1,2],3]" `shouldParse` Pair (Pair (Number 1) (Number 2)) (Number 3)
    it "part one" $
      (readFile "test/Data/Day18.txt" <&> solverPartOne) `shouldReturn` 4140
    it "part two" $
      (readFile "test/Data/Day18.txt" <&> solverPartTwo) `shouldReturn` -2

