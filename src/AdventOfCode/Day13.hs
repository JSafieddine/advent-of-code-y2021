module AdventOfCode.Day13 where

import Control.Monad (forM_)

import Data.Set (Set, fromList, insert, empty, member)
import qualified Data.Set as S (map, foldr)

import Data.Void (Void)

import Text.Megaparsec (Parsec, runParser, (<|>), try, sepEndBy1)
import Text.Megaparsec.Char (char, string, eol)
import Text.Megaparsec.Char.Lexer (decimal)

type Parser = Parsec Void String

type Dot = (Int, Int)
data Fold = FoldX Int | FoldY Int deriving (Show, Eq)

dotParser :: Parser Dot
dotParser = (,) <$> decimal <* char ',' <*> decimal

foldParser :: Parser Fold
foldParser = prefixParser *> (try xFoldParser <|> yFoldParser)
  where
    prefixParser = string "fold along "
    xFoldParser = FoldX <$> (string "x=" *> decimal)
    yFoldParser = FoldY <$> (string "y=" *> decimal)

inputParser :: Parser (Set Dot, [Fold])
inputParser = do
  dots <- sepEndBy1 dotParser eol
  eol
  folds <- sepEndBy1 foldParser eol
  return (fromList dots, folds)

fold :: Fold -> Dot -> Dot
fold (FoldX xf) (x,y) = (xf - abs(xf - x), y)
fold (FoldY yf) (x,y) = (x, yf - abs(yf - y))

foldDots :: Fold -> Set Dot -> Set Dot
foldDots f = S.foldr (insert . fold f) empty

solverPartOne :: String -> Int
solverPartOne input = length . foldDots (head folds) $ dots
  where
    (Right (dots, folds)) = runParser inputParser "" input


getGrid :: Int -> Int -> [[(Int, Int)]]
getGrid xMax yMax = map (\y -> [(x,y) | x <- [0 .. xMax]]) [0 .. yMax]

prettyPrintDots :: Set Dot -> [String]
prettyPrintDots ds = map (map prettyDot) (getGrid xMax yMax)
  where
    xMax = maximum . S.map fst $ ds
    yMax = maximum . S.map snd $ ds
    prettyDot pos = if pos `member` ds then '#' else '.'

applyAllFolds :: Set Dot -> [Fold] -> Set Dot
applyAllFolds = foldl (flip foldDots)

solverPartTwo :: String -> IO ()
solverPartTwo input = forM_ (prettyPrintDots $ applyAllFolds dots folds) print
  where
    (Right (dots, folds)) = runParser inputParser "" input


day13 :: IO ()
day13 = do
  input <- readFile "src/Data/Day13.txt"
  putStrLn "Day 13"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStrLn "Part 2: "
  solverPartTwo input


