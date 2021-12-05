{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day5 where

import Control.Applicative.Combinators (many)

import Data.Void (Void)
import Data.Map (Map, elems, empty, insertWith)

import Text.Megaparsec.Char (punctuationChar, string, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec (runParser, Parsec)

type Parser = Parsec Void String

type FloorMap = Map (Int, Int) Int

type Point = (Int, Int)

data Line = Line {_x1 :: Int, _y1 :: Int, _x2 :: Int, _y2 :: Int} deriving (Show, Eq)

lineParser :: Parser Line
lineParser = Line <$>
  decimal <* punctuationChar <*>
  decimal <* string " -> " <*>
  decimal <* punctuationChar <*>
  decimal <* eol

inputParser :: Parser [Line]
inputParser = many lineParser

filterDiagonals :: [Line] -> [Line]
filterDiagonals = filter filterFunc
  where
    filterFunc l = isHorizontal l || isVertical l

isHorizontal :: Line -> Bool
isHorizontal l = _y1 l == _y2 l

isVertical :: Line -> Bool
isVertical l = _x1 l == _x2 l

lineToPoints :: Line -> [Point]
lineToPoints l
  | isHorizontal l = map (,_y1 l) . xRange $ l
  | isVertical l   = map (_x1 l,) . yRange $ l
  | otherwise      = zip (xRange l) (yRange l)

xRange :: Line -> [Int]
xRange l
  | _x1 l < _x2 l = [_x1 l .. _x2 l]
  | otherwise     = reverse [_x2 l .. _x1 l ]

yRange :: Line -> [Int]
yRange l
  | _y1 l < _y2 l = [_y1 l .. _y2 l]
  | otherwise     = reverse [ _y2 l .. _y1 l]

insertLine :: FloorMap -> Line -> FloorMap
insertLine fm l = foldl foldFunc fm linePoints
  where
    foldFunc fm' k = insertWith (+) k 1 fm'
    linePoints = lineToPoints l

countOverlappingPoint :: FloorMap -> Int
countOverlappingPoint = length . filter (>1) . elems


solverPartOne :: String -> Int
solverPartOne input = countOverlappingPoint . foldl insertLine empty . filterDiagonals $ lines
  where
    (Right lines) = runParser inputParser "" input

solverPartTwo :: String -> Int
solverPartTwo input = countOverlappingPoint . foldl insertLine empty $ lines
  where
    (Right lines) = runParser inputParser "" input

day5 :: IO ()
day5 = do
  input <- readFile "src/Data/Day5.txt"
  putStrLn "Day 5"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input
