module AdventOfCode.Day3Spec (spec) where

import Data.Functor
import Test.Hspec

import AdventOfCode.Day3 (getGammaBit, getGammaBits, invertBits, binToDec, solverPartOne, solverPartTwo, inputParser, getOxygenGeneratorRating, getCO2ScrubberRating)


spec :: Spec
spec = do
  describe "test day 3" $ do
    it "test getGammaBit" $
      getGammaBit [1,0,1,1,0,1] `shouldBe` 1
    it "test getGammaBits" $
      getGammaBits [[1,0,0], [1,1,0], [1,1,1]] `shouldBe` [1, 1, 0]
    it "test invertBits" $
      invertBits [1,0,1,0] `shouldBe` [0,1,0,1]
    it "test binToDec" $
      binToDec [1,1,1] `shouldBe` 7
    it "part one" $
      (readFile "test/Data/Day3.txt" <&> solverPartOne) `shouldReturn` 198
    it "test getOxygenGeneratorRating" $
      (readFile "test/Data/Day3.txt" <&> (getOxygenGeneratorRating . inputParser)) `shouldReturn` 23
    it "test getCO2ScrubberRating" $
      (readFile "test/Data/Day3.txt" <&> (getCO2ScrubberRating . inputParser)) `shouldReturn` 10
    it "part two" $
      (readFile "test/Data/Day3.txt" <&> solverPartTwo) `shouldReturn` 230