module AdventOfCode.Day14 where

import Control.Applicative.Combinators (some, someTill)

import Data.List (group, sort)
import Data.Map (Map, fromList, (!))
import Data.Void (Void)


import Text.Megaparsec (runParser, Parsec, eof)
import Text.Megaparsec.Char (letterChar, eol, string)
import Control.Applicative (Applicative(liftA2))

type Parser = Parsec Void String

type InsertionRules = Map String Char

templateParser :: Parser String
templateParser = letterChar `someTill` eol

insertionRuleParser :: Parser (String, Char)
insertionRuleParser = (,) <$> (letterChar `someTill` string " -> ") <*> letterChar <* eol

inputParser :: Parser (String, InsertionRules)
inputParser = do
  template <- templateParser
  eol
  insertionRules <- some insertionRuleParser
  return (template, fromList insertionRules)

templatePairs :: String -> [String]
templatePairs [] = []
templatePairs [_] = []
templatePairs (a:b:rest) = [a, b] : templatePairs (b:rest)

step :: InsertionRules -> String -> String
step ir = liftA2 intersperse id (map (ir !) . templatePairs)

intersperse :: String -> String -> String
intersperse (t:template) (r:rules) = t:r: intersperse template rules
intersperse t []                   = t
intersperse [] r                   = r


quantityDiff :: String -> Int
quantityDiff = liftA2 (-) maximum minimum . map length . group . sort

solverPartOne :: String -> Int
solverPartOne input = quantityDiff $ iterate (step ir) template !! 10
  where
    (Right (template, ir)) = runParser inputParser "" input


data Polymerizer
  = Polymerizer
  { _template        :: String
  , _insertionRules  :: InsertionRules
  , _pairHistogram   :: Map String Int
  , _singleHistogram :: Map Char Int
  , _step            :: Int
  } deriving (Show)

solverPartTwo :: String -> Int
solverPartTwo input = -1 -- quantityDiff $ iterate (step ir) template !! 40
  where
    (Right (template, ir)) = runParser inputParser "" input

day14 :: IO ()
day14 = do
  input <- readFile "src/Data/Day14.txt"
  putStrLn "Day 14"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

