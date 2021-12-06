{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day6 where

import Control.Applicative.Combinators (sepBy)

import Data.Either (partitionEithers)
import Data.Void (Void)

import Data.List (sort, group)
import Data.Map (Map, empty, insertWith, fromList, foldrWithKey, foldr)

import Text.Megaparsec (Parsec, runParser, eof)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (punctuationChar, eol)

type Parser = Parsec Void String

inputParser :: Parser [Int]
inputParser = (decimal `sepBy` punctuationChar) <* eol

type FishMap = Map Int Int

initFishMap :: [Int] -> FishMap
initFishMap = fromList . map mapFunc . group . sort
  where
    mapFunc fish = (head fish, length fish)

insertStep :: Int -> Int -> FishMap -> FishMap
insertStep 0 numOfFish = insertWith (+) 8 numOfFish . insertWith (+) 6 numOfFish
insertStep n numOfFish = insertWith (+) (pred n) numOfFish

stepFishMap :: FishMap -> FishMap
stepFishMap = foldrWithKey insertStep empty

solverPartOne :: String -> Int
solverPartOne input = sum $ (iterate stepFishMap . initFishMap $ fish) !! 80
  where
    (Right fish) = runParser inputParser "" input

solverPartTwo :: String -> Int
solverPartTwo input = sum $ (iterate stepFishMap . initFishMap $ fish) !! 256 
  where
    (Right fish) = runParser inputParser "" input

day6 :: IO ()
day6 = do
  input <- readFile "src/Data/Day6.txt"
  putStrLn "Day 6"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

