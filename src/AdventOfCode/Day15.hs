module AdventOfCode.Day15 where

import Data.Char (digitToInt)
import Data.Vector (Vector, fromList)

type RiskMap = Vector (Vector Int)

inputParser :: String -> RiskMap
inputParser = fromList . map (fromList . map digitToInt) . init . lines

coordToNode :: (Int, Int) -> (Int,Int) -> Int
coordToNode (xBound, yBound) (x,y) = x + (y-1)*xBound

nodeToCoord :: (Int, Int) -> Int -> (Int, Int)
nodeToCoord (xBound, yBound) node = (node `mod` xBound, 1 + node `div` xBound)


solverPartOne :: String -> Int
solverPartOne _ = -1

solverPartTwo :: String -> Int
solverPartTwo _ = -1
