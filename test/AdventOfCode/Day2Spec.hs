module AdventOfCode.Day2Spec (spec) where

import Data.Functor ((<&>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

import AdventOfCode.Day2


spec :: Spec
spec = do
  describe "test day 2" $ do
    it "commandParser forward 30" $
      parse commandParser "" "forward 30" `shouldParse` Forward 30
    it "commandParser up 20" $
      parse commandParser "" "up 20" `shouldParse` Up 20
    it "commandParser down 10" $
      parse commandParser "" "down 10" `shouldParse` Down 10
    it "inputParser" $ do
      input <- readFile "test/Data/Day2.txt"
      parse inputParser "" input `shouldParse` [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]
    it "part one" $
      (readFile "test/Data/Day2.txt" <&> solverPartOne) `shouldReturn` 150
    it "part two" $
      (readFile "test/Data/Day2.txt" <&> solverPartTwo) `shouldReturn` 900
