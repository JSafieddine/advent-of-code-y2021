{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day10 where


import Data.Maybe (mapMaybe)

import Data.Either (lefts, rights)
import Data.List (sort)


isOpener :: Char -> Bool
isOpener c
  | c == '('  = True
  | c == '['  = True
  | c == '{'  = True
  | c == '<'  = True
  | otherwise = False

isMatch :: Char -> Char -> Bool
isMatch '(' ')' = True
isMatch '[' ']' = True
isMatch '{' '}' = True
isMatch '<' '>' = True
isMatch _ _     = False

getSyntaxCheckerScore :: Char -> Int
getSyntaxCheckerScore ')' = 3
getSyntaxCheckerScore ']' = 57
getSyntaxCheckerScore '}' = 1197
getSyntaxCheckerScore '>' = 25137
getSyntaxCheckerScore _   = error "Something went wrong!"

parseLine' :: (String, String) -> Either Int (String, String)
parseLine' ([], stack) = Right ([], stack)
parseLine' (c:rest, stack)
  | isOpener c = parseLine' (rest, c:stack)
  | otherwise  = if isMatch (head stack) c
                    then parseLine' (rest, tail stack)
                    else Left . getSyntaxCheckerScore $ c

parseLine :: String -> Either Int String
parseLine input = fmap snd . parseLine' $ (input, [])

solverPartOne :: String -> Int
solverPartOne = sum . lefts . map parseLine . lines

getAutoCompleterScore :: String -> Int
getAutoCompleterScore  = foldl foldFunc 0
  where
    score '(' = 1
    score '[' = 2
    score '{' = 3
    score '<' = 4
    score _   = error "Something went wrong!"
    foldFunc acc c = acc * 5 + score c

solverPartTwo :: String -> Int
solverPartTwo input = scores !! (length scores `div` 2)
  where
    scores = sort . map getAutoCompleterScore  . rights . map parseLine . lines $ input

day10 :: IO ()
day10 = do
  input <- readFile "src/Data/Day10.txt"
  putStrLn "Day 10"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input
