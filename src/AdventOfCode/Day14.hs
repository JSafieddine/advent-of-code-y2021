module AdventOfCode.Day14 where

import Control.Applicative.Combinators (some, someTill)
import Control.Monad.State (State, MonadState (get, put), replicateM, evalState)

import Data.List (group, sort)
import Data.Map (Map, fromList, (!), foldrWithKey, insertWith, empty, elems, fromListWith)
import Data.Void (Void)


import Text.Megaparsec (runParser, Parsec, eof)
import Text.Megaparsec.Char (letterChar, eol, string)
import Control.Applicative (Applicative(liftA2))

type Parser = Parsec Void String

type InsertionRules = Map String Char
type PairHistogram = Map String Int
type SingleHistogram = Map Char Int

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


updateSingleHistogram :: (InsertionRules, PairHistogram, SingleHistogram) -> SingleHistogram
updateSingleHistogram (ir, ph, sh) = foldrWithKey foldFunc sh ph
  where
    foldFunc p h = insertWith (+) (ir ! p) h

updatePair :: InsertionRules -> String -> (String, String)
updatePair ir pair@[a,b] = (a:[c], c:[b])
  where
    c = ir ! pair
updatePair _ _ = error "Invalid input"

foldFunc :: InsertionRules -> String -> Int -> PairHistogram -> PairHistogram
foldFunc ir p h = insertWith (+) p2 h . insertWith (+) p1 h
  where
    (p1, p2) = updatePair ir p

updatePairHistogram :: (InsertionRules, PairHistogram, SingleHistogram) -> PairHistogram
updatePairHistogram (ir, ph, sh) = foldrWithKey (foldFunc ir) empty ph

quantityDiff2 :: SingleHistogram -> Int
quantityDiff2 = liftA2 (-) maximum minimum . elems

step2 :: State (InsertionRules, PairHistogram, SingleHistogram) Int
step2 = do
  myState@(ir, ph, sh) <- get
  let sh' = updateSingleHistogram myState
  let ph' = updatePairHistogram myState
  put (ir, ph', sh')
  return . quantityDiff2 $ sh'




solverPartTwo :: String -> Int
solverPartTwo input = last $ evalState (replicateM 40 step2) (ir, pairHist, singleHist)
  where
    (Right (template, ir)) = runParser inputParser "" input
    pairs = templatePairs template
    pairHist = fromListWith (+) $ zip pairs (repeat 1)
    singleHist = fromListWith (+) $ zip template (repeat 1)

day14 :: IO ()
day14 = do
  input <- readFile "src/Data/Day14.txt"
  putStrLn "Day 14"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

