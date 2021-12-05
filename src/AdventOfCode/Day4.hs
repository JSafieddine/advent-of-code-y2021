{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module AdventOfCode.Day4 where

import Control.Applicative.Combinators
    ( sepBy, skipMany, some, count )

import Data.Either (fromLeft, partitionEithers)
import Data.Matrix (Matrix, fromLists, getCol, ncols, getRow, nrows)
import Data.Vector (Vector)
import Data.Void (Void)


import Text.Megaparsec (Parsec, (<|>), many, manyTill, eof, runParser)
import Text.Show (Show)
import Text.Megaparsec.Char (space, string, eol, separatorChar, spaceChar, punctuationChar, newline, char)
import Text.Megaparsec.Char.Lexer (decimal, symbol)
import Control.Monad (forM, mapM)

type Parser = Parsec Void String

type BingoNumbers = [Int]
type BingoField = [[Int]]

bingoNumberParser :: Parser BingoNumbers
bingoNumberParser = (decimal `sepBy` punctuationChar) <* eol

bingoFieldLineParser :: Parser [Int]
bingoFieldLineParser = (many (char ' ') *> decimal) `manyTill` eol

bingoFieldParser :: Parser BingoField
bingoFieldParser = bingoFieldLineParser `manyTill` eol


inputParser :: Parser (BingoNumbers, [BingoField])
inputParser = do
  bingoNumbers <- bingoNumberParser <* eol
  bingoFields <- bingoFieldParser `manyTill` eof
  return (bingoNumbers, bingoFields)

type BingoMatrix = Matrix (Int, Bool)

initBingoMatrix :: BingoField -> BingoMatrix
initBingoMatrix = fmap (, False) . fromLists

drawBingoNumber :: Int -> BingoMatrix -> Either Int BingoMatrix
drawBingoNumber n bm = if win then Left $ calculateScore n bm' else Right bm'
  where
    bm' = fmap (\(n', m) -> if n == n' then (n', True) else (n', m)) bm
    win = checkCols bm' || checkRows bm'

checkDirection :: Int -> (Int -> BingoMatrix -> Vector (Int, Bool)) -> BingoMatrix -> Bool
checkDirection n getDir bm = or bDir
  where
    dir = map (`getDir` bm) [1 .. n]
    bDir = map (all ((==True) . snd)) dir

checkCols :: BingoMatrix -> Bool
checkCols bm = checkDirection (ncols bm) getCol bm

checkRows :: BingoMatrix -> Bool
checkRows bm = checkDirection (nrows bm) getRow bm

calculateScore :: Int -> BingoMatrix -> Int
calculateScore n bm = n * sumUnmarked
  where
    sumUnmarked = foldl (\s (n', m) -> if not m then s + n' else s) 0 bm

multiDrawBingNumber :: Int -> [BingoMatrix] -> Either Int [BingoMatrix]
multiDrawBingNumber n = mapM (drawBingoNumber n)

playBingo :: BingoNumbers -> [BingoMatrix] -> Int
playBingo [] bms = error "Something went wrong!"
playBingo (n:bns) bms  = case multiDrawBingNumber n bms of
  (Right bms')    -> playBingo bns bms'
  (Left solution) -> solution

solverPartOne :: String -> Int
solverPartOne input = playBingo bns (map initBingoMatrix bfs)
  where
    (Right (bns, bfs)) = runParser inputParser "" input

multiDrawBingNumber' :: Int -> [BingoMatrix] -> [Either Int BingoMatrix]
multiDrawBingNumber' n = map (drawBingoNumber n)

playBingo' :: BingoNumbers -> [BingoMatrix] -> Int
playBingo' [] _ = error "Something went wrong!"
playBingo' _ [] = error "Something went wrong!"
playBingo' (n:bns) bms = case partitionEithers $ multiDrawBingNumber' n bms of
  ([last], []) -> last
  (_, bms')    -> playBingo' bns bms' 

solverPartTwo :: String -> Int
solverPartTwo input = playBingo' bns (map initBingoMatrix bfs)
  where
    (Right (bns, bfs)) = runParser inputParser "" input

day4 :: IO ()
day4 = do
  input <- readFile "src/Data/Day4.txt"
  putStrLn "Day 4"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

