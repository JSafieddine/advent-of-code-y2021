module AdventOfCode.Day5Spec (spec) where

import Test.Hspec
import Test.Hspec.Megaparsec

import Data.Functor ((<&>))
import Text.Megaparsec (parse)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (spaceChar)
import Control.Applicative.Combinators (sepBy)


import AdventOfCode.Day5


spec :: Spec
spec = do
  describe "test day 5" $ do
    it "test lineParser" $
      parse lineParser "" "0,9 -> 5,9\n" `shouldParse` Line 0 9 5 9
    it "test inputParser" $ do
      input <- readFile "test/Data/Day5.txt"
      parse inputParser "" input `shouldParse` [
          Line  0 9 5 9,
          Line  8 0 0 8,
          Line  9 4 3 4,
          Line  2 2 2 1,
          Line  7 0 7 4,
          Line  6 4 2 0,
          Line  0 9 2 9,
          Line  3 4 1 4,
          Line  0 0 8 8,
          Line  5 5 8 2
        ]

    it "test solverPartOne" $
      (readFile "test/Data/Day5.txt" <&> solverPartOne) `shouldReturn` 5
    it "test solverPartTwo" $
      (readFile "test/Data/Day5.txt" <&> solverPartTwo) `shouldReturn` 12

