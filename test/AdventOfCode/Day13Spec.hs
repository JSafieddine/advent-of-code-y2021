module AdventOfCode.Day13Spec (spec) where

import Data.Functor ((<&>))
import Data.Set (fromList)

import Text.Megaparsec (runParser)

import Test.Hspec
import Test.Hspec.Megaparsec

import AdventOfCode.Day13

testInput :: String
testInput = "3,2\n4,2\n\nfold along y=3\nfold along x=5\n"

expectedOutput = (fromList [(3,2), (4,2)], [FoldY 3, FoldX 5])


spec :: Spec
spec = do
  describe "test day 13" $ do
    it "inputParser test" $ do
      runParser inputParser "" testInput `shouldParse` expectedOutput
    it "part one" $
      (readFile "test/Data/Day13.txt" <&> solverPartOne) `shouldReturn` 17
