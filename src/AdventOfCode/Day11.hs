module AdventOfCode.Day11 where

import Data.Char (digitToInt)
import Data.Vector (Vector, fromList)

type EnergyMap = Vector (Vector Int)

inputParser :: String -> EnergyMap
inputParser = fromList . map (fromList . map digitToInt) . lines

solverPartOne :: String -> Int
solverPartOne _ = -1

solverPartTwo :: String -> Int
solverPartTwo _ = -1
