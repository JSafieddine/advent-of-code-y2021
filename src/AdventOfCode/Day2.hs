{-# LANGUAGE OverloadedStrings #-}

module AdventOfCode.Day2 where

import Text.Megaparsec (Parsec, (<|>), many, eof, runParser)
import Data.Void (Void)
import Text.Show (Show)
import Text.Megaparsec.Char (string, eol)
import Text.Megaparsec.Char.Lexer (decimal)
import System.IO (putStr)

day2 :: IO ()
day2 = do
    input <- readFile "src/Data/Day2.txt"
    putStrLn "Day 2"
    putStr "Part 1: "
    print . solverPartOne $ input
    putStr "Part 2: "
    print . solverPartTwo $ input

type Parser = Parsec Void String

data Command = Forward Int | Down Int | Up Int
    deriving (Show, Eq)

commandParser :: Parser Command
commandParser = forwardParser <|> downParser <|> upParser
    where
        forwardParser = Forward <$> (string "forward " *> decimal)
        downParser = Down <$> (string "down " *>  decimal)
        upParser = Up <$> (string "up " *> decimal)

inputParser :: Parser [Command]
inputParser = many (commandParser <* eol) <* eof

data Position = Position {x :: Int, y :: Int, aim :: Int}
    deriving (Show, Eq)

initialPosition :: Position
initialPosition = Position 0 0 0

followInstruction :: Position -> Command -> Position
followInstruction p c = case c of
  Forward n -> p { x = x p + n}
  Down n    -> p { y = y p + n}
  Up n      -> p { y = y p - n}

evaluatePosition :: Position -> Int
evaluatePosition p = x p * y p

solverPartOne :: String -> Int
solverPartOne input = evaluatePosition $ foldl followInstruction initialPosition commands
    where
        (Right commands) = runParser inputParser "" input

followInstructionWithAim :: Position -> Command -> Position
followInstructionWithAim p c = case c of
  Forward n -> p { x = x p + n, y = y p + aim p * n}
  Down n    -> p { aim = aim p + n}
  Up n      -> p { aim = aim p - n}

solverPartTwo :: String -> Int
solverPartTwo input = evaluatePosition $ foldl followInstructionWithAim initialPosition commands
    where
        (Right commands) = runParser inputParser "" input