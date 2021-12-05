module AdventOfCode.Day4Spec (spec) where


import Test.Hspec
import Test.Hspec.Megaparsec

import Data.Functor ((<&>))
import Text.Megaparsec (parse)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (spaceChar)
import Control.Applicative.Combinators (sepBy)


import AdventOfCode.Day4


spec :: Spec
spec = do
  describe "test day 4" $ do
    it "test bingoNumberParser" $
      parse bingoNumberParser "" "1,2,3,4,30\n" `shouldParse` [1,2,3,4,30]
    it "test bingoFieldParser" $
      parse bingoFieldParser "" " 1  2 10\n 3  3 10\n\n" `shouldParse` 
      [[1, 2, 10], [3, 3, 10]]
    it "inputParser" $ do
      input <- readFile "test/Data/Day4.txt"
      parse inputParser "" input `shouldParse` (
        [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1],
        [
          [
            [22, 13, 17, 11, 0],
            [8, 2, 23, 4, 24],
            [21, 9, 14, 16, 7],
            [6, 10, 3, 18, 5],
            [1, 12, 20, 15, 19]
          ],
          [
            [3, 15, 0, 2, 22],
            [9, 18, 13, 17, 5],
            [19, 8, 7, 25, 23],
            [20, 11, 10, 24, 4],
            [14, 21, 16, 12, 6]
          ],
          [
            [14, 21, 17, 24, 4],
            [10, 16, 15, 9, 19],
            [18, 8, 23, 26, 20],
            [22, 11, 13, 6, 5],
            [2, 0, 12, 3, 7]
          ]
        ])
    it "part one" $
      (readFile "test/Data/Day4.txt" <&> solverPartOne) `shouldReturn` 4512
    it "part two" $
      (readFile "test/Data/Day4.txt" <&> solverPartTwo) `shouldReturn` 1924
