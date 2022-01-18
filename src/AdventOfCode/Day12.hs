module AdventOfCode.Day12 where

import Data.Graph.DGraph (DGraph, fromArcsList, outboundingArcs)
import Data.Graph.Types ( Arc(Arc), (-->), Edge(Edge), (<->) )
import Data.Graph.UGraph (UGraph, fromEdgesList, incidentEdges)

import Data.Char (isLower, isUpper)
import Data.Functor ((<&>))
import Data.Set (Set, notMember, insert, empty, singleton, member)

import Data.Void

import Text.Megaparsec (Parsec, someTill, some, runParser)
import Text.Megaparsec.Char (alphaNumChar, eol, char)

type Parser = Parsec Void String

type Cave = String
type CaveArc = Arc Cave ()
type CaveEdge = Edge Cave ()
type CaveArcPath = [CaveArc]
type CaveEdgePath = [CaveEdge]
type CaveDGraph = DGraph Cave ()
type CaveUGraph = UGraph Cave ()

lineParser :: Parser (Cave, Cave)
lineParser = do
  start <- someTill alphaNumChar (char '-')
  end <- someTill alphaNumChar eol
  return (start, end)

inputParser :: Parser [(Cave, Cave)]
inputParser = some lineParser

buildCaveDGraph :: [(Cave, Cave)] -> CaveDGraph
buildCaveDGraph = fromArcsList . map (uncurry (-->))

buildCaveUGraph :: [(Cave, Cave)] -> CaveUGraph
buildCaveUGraph = fromEdgesList . map (uncurry (<->))

-- extract the out vertex from an arc
outArcVertex :: CaveArc -> Cave
outArcVertex (Arc i o _) = o

-- get all outbound vertices in a directed graph
(~~>) :: CaveDGraph -> Cave -> [Cave]
g ~~> c = map outArcVertex $ outboundingArcs g c

outEdgeVertex :: CaveEdge -> Cave
outEdgeVertex (Edge i o _) = o

-- get all outbound vertices in an undirected graph
(<~>) :: CaveUGraph -> Cave -> [Cave]
g <~> c = map outEdgeVertex $ incidentEdges g c

getAllArcPath' :: CaveDGraph -> Cave -> Cave -> Set Cave -> [CaveArcPath]
getAllArcPath' g s e v
  | s == e    = [[]]
  | otherwise = [s --> i :  path | i <- g ~~> s, i `notMember` v , path <- getAllArcPath' g i e v']
  where
    v' = if isLower . head $ s then insert s v else v

getAllArcPath :: CaveDGraph -> [CaveArcPath]
getAllArcPath g = getAllArcPath' g "start" "end" empty

getAllEdgePath' :: CaveUGraph -> Cave -> Cave -> Set Cave -> [CaveEdgePath]
getAllEdgePath' g s e v
  | s == e    = [[]]
  | otherwise = [ s <-> i : path | i <- g <~> s, i `notMember` v, path <- getAllEdgePath' g i e v']
  where
    v' = if isLower . head $ s then insert s v else v

getAllEdgePath :: CaveUGraph -> [CaveEdgePath]
getAllEdgePath g = getAllEdgePath' g "start" "end" empty

solverPartOne :: String -> Int
solverPartOne input = length . getAllEdgePath $ caveGraph
  where
    (Right caveTuple) = runParser inputParser "" input
    caveGraph         = buildCaveUGraph caveTuple

data Visited = VisitedOnce (Set Cave) | VisitedTwice (Set Cave)

filterVisited :: Visited -> Cave -> Bool
filterVisited _ "start"               = False
filterVisited _ "end"                 = True
filterVisited (VisitedOnce caveSet) _ = True
filterVisited (VisitedTwice caveSet) cave
  | isUpper . head $ cave = True
  | cave `member` caveSet = False
  | otherwise             = True 

updateVisited :: Visited -> Cave -> Visited
updateVisited v "start" = v
updateVisited v@(VisitedOnce caveSet) c
  | isUpper . head $ c = v 
  | c `member` caveSet = VisitedTwice caveSet
  | otherwise          = VisitedOnce (insert c caveSet)
updateVisited v@(VisitedTwice caveSet) c
  | isUpper . head $ c = v 
  | c `member` caveSet = VisitedTwice caveSet
  | otherwise          = VisitedTwice (insert c caveSet)

getAllEdgePath2' :: CaveUGraph -> Cave -> Cave -> Visited -> [CaveEdgePath]
getAllEdgePath2' g s e v
  | s == e    = [[]]
  | otherwise = [s <-> i : path | i <- g <~> s, filterVisited v' i, path <- getAllEdgePath2' g i  e v']
  where
    v' = updateVisited v s

getAllEdgePath2 :: CaveUGraph -> [CaveEdgePath]
getAllEdgePath2 g = getAllEdgePath2' g "start" "end" (VisitedOnce empty)

solverPartTwo :: String -> Int
solverPartTwo input = length . getAllEdgePath2 $ caveGraph
  where
    (Right caveTuple) = runParser inputParser "" input
    caveGraph         = buildCaveUGraph caveTuple

day12 :: IO ()
day12 = do
  input <- readFile "src/Data/Day12.txt"
  putStrLn "Day 12"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

