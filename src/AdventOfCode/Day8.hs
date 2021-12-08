{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day8 where

import Data.Set (Set, fromList, intersection)
import Data.Void (Void)

import Text.Megaparsec (runParser, Parsec, (<|>), eof)
import Text.Megaparsec.Char (char, separatorChar, string, eol,space)
import Control.Applicative.Combinators (sepBy, many, endBy, manyTill)
import Data.Functor (($>))

type Parser = Parsec Void String

data Signal = A | B | C | D | E | F | G
  deriving (Show, Eq, Ord)

type Digit = Set Signal

data FourDigitSevenSegmentDisplay
  = FDSSD
  { _signalPatterns :: [Digit]
  , _fourDigitOutput :: [Digit]
  } deriving (Show, Eq)

signalParser :: Parser Signal
signalParser =  char 'a' $> A
            <|> char 'b' $> B
            <|> char 'c' $> C
            <|> char 'd' $> D
            <|> char 'e' $> E
            <|> char 'f' $> F
            <|> char 'g' $> G

digitParser :: Parser Digit
digitParser = fromList <$> many signalParser

signalPatternParser :: Parser [Digit]
signalPatternParser = (digitParser <* separatorChar) `manyTill` string "| "

digitListParser :: Parser [Digit]
digitListParser = digitParser `sepBy` separatorChar

fourDigitSenvenSegmentDisplayParser :: Parser FourDigitSevenSegmentDisplay
fourDigitSenvenSegmentDisplayParser = FDSSD <$> signalPatternParser <*> digitListParser

inputParser :: Parser [FourDigitSevenSegmentDisplay]
inputParser = many (fourDigitSenvenSegmentDisplayParser <* eol)


solverPartOne :: String -> Int
solverPartOne input = length . filter filterFunc . map length . concatMap _fourDigitOutput $ fdssds
  where
    (Right fdssds) = runParser inputParser "" input
    filterFunc n
      | n == 2 || n == 3 || n== 4 || n == 7 = True
      | otherwise                           = False

getUniqueDigit :: Int -> [Digit] -> Digit
getUniqueDigit 1 = head . filter ((==2) . length)
getUniqueDigit 4 = head . filter ((==4) . length)
getUniqueDigit 7 = head . filter ((==3) . length)
getUniqueDigit 8 = head . filter ((==7) . length)
getUniqueDigit _ = error "Not a unique digit!"

determineDigit :: [Digit] -> Digit -> Int
determineDigit pattern digit
  | length digit == 2 = 1
  | length digit == 3 = 7
  | length digit == 4 = 4
  | length digit == 5 = determineFiveSignalDigit pattern digit
  | length digit == 6 = determineSixSignalDigit pattern digit
  | length digit == 7 = 8
  | otherwise         = error "Invalid Digit!"

determineFiveSignalDigit :: [Digit] -> Digit -> Int
determineFiveSignalDigit pattern digit
  | simWithOne == 2  = 3
  | simWithFour == 2 = 2
  | otherwise        = 5
  where
    simWithOne = length . intersection digit $ getUniqueDigit 1 pattern
    simWithFour = length . intersection digit $ getUniqueDigit 4 pattern

determineSixSignalDigit :: [Digit] -> Digit -> Int
determineSixSignalDigit pattern digit
  | simWithOne == 1  = 6
  | simWithFour == 4 = 9
  | otherwise        = 0
  where
    simWithOne = length . intersection digit $ getUniqueDigit 1 pattern
    simWithFour = length . intersection digit $ getUniqueDigit 4 pattern

determineNumber :: FourDigitSevenSegmentDisplay -> Int
determineNumber fdssd = digitsToNumber . map (determineDigit . _signalPatterns $ fdssd) $ _fourDigitOutput fdssd
  where
    digitsToNumber digits = sum $ zipWith (\a b -> a * 10 ^b) (reverse digits) [0..]


solverPartTwo :: String -> Int
solverPartTwo input = sum . map determineNumber $ fdssds
  where
    (Right fdssds) = runParser inputParser "" input

day8 :: IO ()
day8 = do
  input <- readFile "src/Data/Day8.txt"
  putStrLn "Day 8"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input
