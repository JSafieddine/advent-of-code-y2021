{-# LANGUAGE TupleSections #-}
module AdventOfCode.Day11 where

import Control.Monad (join)

import Data.Char (digitToInt)
import Data.Vector (Vector, fromList, imap, ifoldr, (!))

import Lens.Micro.Platform (mapped, (%~), _1, _2, ix, (.~), (&), (+~))


type EnergyMap = Vector (Vector (Int, Bool))

inputParser :: String -> EnergyMap
inputParser = fromList . map (fromList . map ((,False) . digitToInt)) . filter (not . null) . lines

globalEnergyIncrease :: EnergyMap -> EnergyMap
globalEnergyIncrease = mapped . mapped . _1 %~ succ

getFlashingPositions :: EnergyMap -> [(Int, Int)]
getFlashingPositions = ifoldr outerFoldFunc []
  where
    outerFoldFunc iRow col acc = acc ++ ifoldr (innerFoldFunc iRow) [] col
    innerFoldFunc iRow iCol (el, flashed) acc = if el > 9 && not flashed then (iRow, iCol):acc else acc

getNeighbourhood :: EnergyMap -> (Int, Int) -> [(Int, Int)]
getNeighbourhood em (x, y) = filter filterFunc [(x - x0, y - y0) | x0 <- [-1 .. 1], y0 <- [-1 .. 1], x0 /= 0 || y0 /= 0]
  where
    filterFunc (x0, y0) = 0 <= x0 && x0 < length (em ! y) && 0 <= y0 && y0 < length em

increaseEnergyLevel :: EnergyMap -> (Int, Int) -> EnergyMap
increaseEnergyLevel em (x, y) = em & ix x . ix y . _1 +~ 1

setFlashed :: EnergyMap -> (Int, Int)-> EnergyMap
setFlashed em (x, y) = em & ix x . ix y . _2 .~ True

simulateFlashes :: (EnergyMap, Int) -> (EnergyMap, Int)
simulateFlashes (em, f) = if null fp
    then (em, f)
    else simulateFlashes (poweredUpEm, f + length fp)
  where
    fp = getFlashingPositions em
    flashedEm = foldl setFlashed em fp
    neighbourhood = concatMap (getNeighbourhood flashedEm) fp
    poweredUpEm = foldl increaseEnergyLevel flashedEm neighbourhood

resetEnergyLevel :: EnergyMap -> EnergyMap
resetEnergyLevel = mapped . mapped %~ resetFunc
  where
    resetFunc (em, _) = (if em > 9 then 0 else em, False)

step :: (EnergyMap, Int) -> (EnergyMap, Int)
step (em, f) = (resettedEm, f')
  where
    gei = globalEnergyIncrease em
    (flashedEm, f') = simulateFlashes (gei, f)
    resettedEm = resetEnergyLevel flashedEm

solverPartOne :: String -> Int
solverPartOne input = snd (iterate step (em, 0) !! 100)
  where
    em = inputParser input

findFixPoint' :: (EnergyMap, Int) -> (EnergyMap, Int)
findFixPoint' (em, s) = if octoNum == f then (em', s + 1) else findFixPoint' (em', s + 1)
  where
    (em', f) = step (em, 0)
    octoNum = length em * length (em ! 0)

findFixPoint :: EnergyMap -> Int
findFixPoint em = snd . findFixPoint' $ (em, 0)

solverPartTwo :: String -> Int
solverPartTwo input = findFixPoint em 
  where
    em = inputParser input

day11 :: IO ()
day11 = do
  input <- readFile "src/Data/Day11.txt"
  putStrLn "Day 11"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

