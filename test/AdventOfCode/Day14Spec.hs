module AdventOfCode.Day14Spec (spec) where

import Data.Functor ((<&>))

import Text.Megaparsec (runParser)

import Test.Hspec
import Test.Hspec.Megaparsec

import AdventOfCode.Day14


spec :: Spec
spec = do
  describe "test day 14" $ do
    it "test insertionRuleParser" $
      runParser insertionRuleParser "" "CD -> H\n" `shouldParse` ("CD", 'H')
    it "part one" $
      (readFile "test/Data/Day14.txt" <&> solverPartOne) `shouldReturn` 1588
    it "part two" $
      (readFile "test/Data/Day14.txt" <&> solverPartTwo) `shouldReturn` 2188189693529
