module AdventOfCode.Day19 where

import Data.Void (Void)

import Text.Megaparsec (runParser, Parsec, endBy, some, someTill)
import Text.Megaparsec.Char.Lexer (signed, decimal)
import Text.Megaparsec.Char (hspace, string, eol, char)

type Parser = Parsec Void String

type Position = (Int, Int, Int)

data Scanner = Scanner {_number :: Int, _beacons :: [Position]} deriving (Show, Eq)

positionParser :: Parser Position
positionParser = (,,)
              <$> (signed hspace decimal <* char ',')
              <*> (signed hspace decimal <* char ',')
              <*> signed hspace decimal
              <* eol

scannerParser :: Parser Scanner
scannerParser = do
  string "--- scanner "
  n <- decimal
  string " ---"
  eol
  positions <- positionParser `someTill` eol
  return $ Scanner n positions

inputParser :: Parser [Scanner]
inputParser = some scannerParser

 


solverPartOne :: String -> Int
solverPartOne _ = -1

solverPartTwo :: String -> Int
solverPartTwo _ = -1

