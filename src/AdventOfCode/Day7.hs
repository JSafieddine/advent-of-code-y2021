{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day7 where

import Control.Applicative.Combinators (sepBy)

import Data.Void (Void)

import Data.List (sort, minimumBy)

import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (punctuationChar, eol)

type Parser = Parsec Void String

inputParser :: Parser [Int]
inputParser = (decimal `sepBy` punctuationChar) <* eol

median :: [Int] -> Int
median [] = error "Invalid input!"
median x
  | even n    = round $ fromIntegral ((xs !! (n `div` 2)) + (xs !! ((n - 1) `div` 2))) / 2.0
  | otherwise = xs !! (n `div` 2)
  where
    n = length x
    xs = sort x

getFuelConsumption :: [Int] -> Int -> Int
getFuelConsumption crabSubs posOfAlign = sum . map (abs . (-) posOfAlign) $ crabSubs

getSingleFuelConsumption :: Int -> Int
getSingleFuelConsumption n = n * (n+1) `div` 2

getFuelConsumption' :: [Int] -> Int -> Int
getFuelConsumption' crabSubs posOfAlign = sum . map (getSingleFuelConsumption . abs . (-) posOfAlign) $ crabSubs

-- Why does the median work here?
solverPartOne :: String -> Int
solverPartOne input = getFuelConsumption crabSubs (median crabSubs)
  where
    (Right crabSubs) = runParser inputParser "" input

-- First thought was to use the arithmetic mean (rounded) here,
-- that didn't work or did I make a mistake implementing it?
solverPartTwo :: String -> Int
solverPartTwo input = minimum . map (getFuelConsumption' crabSubs) $ [minimum crabSubs .. maximum crabSubs]
   where
     (Right crabSubs) = runParser inputParser "" input
     minFunc a b
       | snd a < snd b = LT
       | snd a > snd b = GT
       | otherwise     = EQ

day7 :: IO ()
day7 = do
  input <- readFile "src/Data/Day7.txt"
  putStrLn "Day 7"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

