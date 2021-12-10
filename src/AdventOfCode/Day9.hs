module AdventOfCode.Day9 where

import Data.Char (digitToInt)
import Data.List (nub, sort)
import Data.Map (Map, fromList, lookup, foldrWithKey)
import Data.Maybe (mapMaybe, fromJust)

import Prelude hiding (lookup)

type Coords = (Int, Int)
type HightMap = Map Coords Int


inputParser :: String -> [[Int]]
inputParser = map (map digitToInt) . init . lines

grid :: [[Coords]]
grid = map (\n -> [ (n,x) | x <- [1..]]) [1..]

initHightMap :: [[Int]] -> HightMap
initHightMap = fromList . concat . zipWith zip grid

getAdjacent :: Coords -> [Coords]
getAdjacent (x,y) = [(x - 1, y), (x + 1, y), (x, y -1), (x, y + 1)]

isLowPoint :: HightMap -> Coords -> Bool
isLowPoint hm coords = hight < minimum surrounding
  where
    hight       = fromJust . lookup coords $ hm
    surrounding = mapMaybe mapFunc adjacents
    adjacents   = getAdjacent coords
    mapFunc c   = lookup c hm

calcRiskLevel :: HightMap -> Int
calcRiskLevel hm = foldrWithKey foldFunc 0 hm
  where
    foldFunc c h acc = if isLowPoint hm c
                          then h + 1 + acc
                          else acc

solverPartOne :: String -> Int
solverPartOne = calcRiskLevel . initHightMap . inputParser

getLowPointCoords :: HightMap -> [Coords]
getLowPointCoords hm = foldrWithKey foldFunc [] hm
  where
    foldFunc c _ acc = if isLowPoint hm c
                          then c:acc
                          else acc

getHigherAdjacency :: HightMap -> Coords -> [Coords]
getHigherAdjacency hm coords = case lookup coords hm of
   (Just hight) -> nub $ coords : concatMap (getHigherAdjacency hm) adjacents
   Nothing      -> []
  where
    hight        = fromJust . lookup coords $ hm
    adjacents    = filter filterFunc . getAdjacent $ coords
    filterFunc c = case lookup c hm of
                     (Just h) -> h > hight && h < 9
                     Nothing  -> False

solverPartTwo :: String -> Int
solverPartTwo input = product . take 3 . reverse . sort . map (length . getHigherAdjacency hm) . getLowPointCoords $ hm
  where
    hm = initHightMap . inputParser $ input



day9 :: IO ()
day9 = do
  input <- readFile "src/Data/Day9.txt"
  putStrLn "Day 9"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

