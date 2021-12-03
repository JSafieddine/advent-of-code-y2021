module AdventOfCode.Day3 where

import Data.Char (digitToInt)
import Data.List (transpose, sort, group)
import Data.Bits (xor)


day3 :: IO ()
day3 = do
    input <- readFile "src/Data/Day3.txt"
    putStrLn "Day3"
    putStr "Part 1: "
    print . solverPartOne $ input
    putStr "Part 2: "
    print . solverPartTwo $ input

inputParser :: String -> [[Int]]
inputParser = map (map digitToInt) . lines

getGammaBits :: [[Int]] -> [Int]
getGammaBits = map getGammaBit . transpose

getGammaBit :: [Int] -> Int
getGammaBit bits = case group . sort $ bits of
  [zeros, ones] -> if length zeros > length ones
      then 0
      else 1
  [bits]        -> head bits
  _             -> error "Invalid input!"

invertBits :: [Int] -> [Int]
invertBits = map (xor 1)

binToDec :: [Int] -> Int
binToDec = foldl foldFunc 0 . zip [0..] . reverse
    where
        foldFunc acc (i, b) = acc + b * 2 ^ i

solverPartOne :: String -> Int
solverPartOne input = binToDec gammaBits * binToDec epsilonBits
    where
        bits = inputParser input
        gammaBits = getGammaBits bits
        epsilonBits = invertBits gammaBits

recursiveFilter :: [[Int]] -> ([[Int]] -> [Int]) -> Int -> [Int]
recursiveFilter bits bitFunc pos
    | length step > 1  = recursiveFilter step bitFunc (pos+1)
    | length step == 1 = head step
    | otherwise        = error "Something went wrong!"
    where
        gammaRate = bitFunc bits
        filterFunc binNum = binNum !! pos == gammaRate !! pos
        step = filter filterFunc bits

getOxygenGeneratorRating :: [[Int]] -> Int
getOxygenGeneratorRating bits = binToDec $ recursiveFilter bits getGammaBits 0

getCO2ScrubberRating :: [[Int]] -> Int
getCO2ScrubberRating bits = binToDec $ recursiveFilter bits getEpsilonBits 0
    where
        getEpsilonBits = invertBits . getGammaBits

solverPartTwo :: String -> Int
solverPartTwo input = oxygen * co2
    where
        bits = inputParser input
        oxygen = getOxygenGeneratorRating bits
        co2 = getCO2ScrubberRating bits