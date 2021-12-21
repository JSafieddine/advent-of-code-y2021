module AdventOfCode.Day18 where

import Control.Monad.State 

import Data.Void (Void)

import Text.Megaparsec (runParser, Parsec, (<|>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

data SnailFishNumber
  = Pair {_left :: SnailFishNumber, _right :: SnailFishNumber}
  | Number {_number :: Int}
  deriving (Show, Eq)

isNumber :: SnailFishNumber -> Bool
isNumber (Number _) = True
isNumber _          = False

type SnailFishNumberState = (SnailFishNumber, [SnailFishNumber], Int)

left :: State SnailFishNumberState (Either String SnailFishNumber)
left = do
  (sfn, trail, depth) <- get
  if isNumber sfn
    then return . Left $ "Can't go left!"
    else do
        put (_left sfn, sfn: trail, depth + 1)
        return . Right . _left $ sfn



pairParser :: Parser SnailFishNumber
pairParser = Pair <$> (char '[' *> snailFishNumberParser) <*> (char ',' *> snailFishNumberParser <* char ']')

snailFishNumberParser :: Parser SnailFishNumber
snailFishNumberParser = (Number <$> decimal) <|> pairParser


solverPartOne :: String -> Int
solverPartOne _ = -1

solverPartTwo :: String -> Int
solverPartTwo _ = -1

