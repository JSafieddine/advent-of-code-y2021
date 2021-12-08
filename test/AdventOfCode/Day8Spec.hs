module AdventOfCode.Day8Spec (spec) where

import Data.Set
import Data.Functor ((<&>))

import Text.Megaparsec (runParser)

import Test.Hspec
import Test.Hspec.Megaparsec


import AdventOfCode.Day8

spec :: Spec
spec = do
  describe "test day 8" $ do
    it "test signalParser" $
      runParser signalParser "" "a" `shouldParse` A
    it "test digitParser" $
      runParser digitParser "" "abcdef" `shouldParse` fromList [A,B,C,D,E,F]
    it "test digitListParser" $
      runParser digitListParser "" "be cfbegad cbdgef" `shouldParse` (fromList <$> [[B,E], [C,F,B,E,G,A,D], [C,B,D,G,E,F]])
    it "test fourDigitSenvenSegmentDisplayParser" $
      runParser fourDigitSenvenSegmentDisplayParser "" "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe" `shouldParse`
        FDSSD (fromList <$> [
          [B,E],
          [C,F,B,E,G,A,D],
          [C,B,D,G,E,F],
          [F,G,A,E,C,D],
          [C,G,E,B],
          [F,D,C,G,E],
          [A,G,E,B,F,D],
          [F,E,C,D,B],
          [F,A,B,C,D],
          [E,D,B]
        ]) (fromList <$> [
          [F,D,G,A,C,B,E],
          [C,E,F,D,B],
          [C,E,F,B,G,D],
          [G,C,B,E]
        ])
    it "test inputParser" $
      runParser inputParser "" "abc def | abc be\ndef abc | adb cfg\n" `shouldParse` 
        [
          FDSSD (fromList <$> [[A,B,C], [D,E,F]]) (fromList <$> [[A,B,C], [B,E]]),
          FDSSD (fromList <$>  [[D,E,F], [A,B,C]]) (fromList <$> [[A,D,B], [C,F,G]])
        ]
    it "part one" $
      (readFile "test/Data/Day8.txt" <&> solverPartOne) `shouldReturn` 26
    it "part two" $
      (readFile "test/Data/Day8.txt" <&> solverPartTwo) `shouldReturn` 61229
