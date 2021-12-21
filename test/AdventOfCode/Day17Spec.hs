module AdventOfCode.Day17Spec (spec) where

import Control.Monad.State (evalState)

import Data.Functor ((<&>))

import Text.Megaparsec (runParser)

import Test.Hspec
import Test.Hspec.Megaparsec (shouldParse)

import AdventOfCode.Day17


spec :: Spec
spec = do
  describe "test day 17" $ do
    it "test inputParser" $ do
      input <- readFile "test/Data/Day17.txt"
      runParser inputParser "" input `shouldParse` TargetArea 20 30 (-10) (-5)
    it "test step" $
      let tg = TargetArea 20 30 (-10) (-5) in
          evalState (step tg) (initialState (6,9)) `shouldBe` Just 45
    it "part one" $
      (readFile "test/Data/Day17.txt" <&> solverPartOne) `shouldReturn` 45
    it "part two" $
      (readFile "test/Data/Day17.txt" <&> solverPartTwo) `shouldReturn` 112

