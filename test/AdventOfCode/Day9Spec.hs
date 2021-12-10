module AdventOfCode.Day9Spec (spec) where

import Test.Hspec

import Data.Functor ((<&>))


import AdventOfCode.Day9


spec :: Spec
spec =
  describe "test day 9" $ do
    it "inputParser" $
      (readFile "test/Data/Day9.txt" <&> inputParser) `shouldReturn` [
          [2,1,9,9,9,4,3,2,1,0],
          [3,9,8,7,8,9,4,9,2,1],
          [9,8,5,6,7,8,9,8,9,2],
          [8,7,6,7,8,9,6,7,8,9],
          [9,8,9,9,9,6,5,6,7,8]
        ]
    it "test isLowPoint" $ do
      input <- readFile "test/Data/Day9.txt"
      let hights = inputParser input
      let hightMap = initHightMap hights
      isLowPoint hightMap (1,1) `shouldBe` False
      isLowPoint hightMap (1, 10 ) `shouldBe` True
    it "part one" $
      (readFile "test/Data/Day9.txt" <&> solverPartOne) `shouldReturn` 15
    it "part two" $
      (readFile "test/Data/Day9.txt" <&> solverPartTwo) `shouldReturn` 1134
