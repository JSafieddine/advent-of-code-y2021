module AdventOfCode.Day17 where

import Control.Monad.State

import Data.Void (Void)

import Text.Megaparsec (runParser, Parsec)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Char (string, hspace)
import Control.Applicative (liftA2)
import Data.Maybe (mapMaybe)

type Parser = Parsec Void String

data TargetArea = TargetArea {_xMin :: Int, _xMax :: Int, _yMin :: Int, _yMax :: Int} deriving (Show, Eq)

data ProbeState = InFlight | InTarget | OverShot

type Position = (Int, Int)
type Velocity = (Int, Int)

inputParser :: Parser TargetArea
inputParser = TargetArea
           <$> (string "target area: x=" *> signedDecimal)
           <*> (string ".." *> signedDecimal)
           <*> (string ", y=" *> signedDecimal)
           <*> (string ".." *> signedDecimal)
    where
      signedDecimal = signed hspace decimal

inTargetArea :: TargetArea -> Position -> ProbeState
inTargetArea tg (x, y)
  | _xMin tg <= x && x <= _xMax tg && _yMin tg <= y && y <= _yMax tg = InTarget
  | x >= _xMax tg || y <= _yMin tg                                   = OverShot
  | otherwise                                                        = InFlight

initialState :: Velocity -> (Position, Velocity, Int)
initialState vel = ((0,0), vel, 0)

step :: TargetArea -> State (Position, Velocity, Int) (Maybe Int)
step tg = do
  (position@(xPos, yPos), (xVel, yVel), maxHight) <- get
  case inTargetArea tg position of
    OverShot -> return Nothing
    InTarget -> return . Just $ maxHight
    InFlight -> put ((xPos + xVel, yPos + yVel), (updateXVel xVel, yVel - 1), max maxHight (yPos + yVel)) >> step tg
  where
    updateXVel n
      | n > 0     = pred n
      | n < 0     = succ n
      | otherwise = 0

validXStartVelocities :: TargetArea -> [Int]
validXStartVelocities tg = [minValue .. _xMax tg]
  where
    minValue = ceiling $ -1/2 + sqrt (1/4 + 2 * (fromIntegral . _xMin $ tg))


validYStartVelocities :: TargetArea -> [Int]
validYStartVelocities tg = [yMin .. yMax]
  where
    yMin = _yMin tg
    yMax = maximum . map abs $ [_yMin tg, _yMax tg]

validVelocities :: TargetArea -> [Velocity]
validVelocities tg = liftA2 (,) (validXStartVelocities tg) (validYStartVelocities tg)



solverPartOne :: String -> Int
solverPartOne input = maximum . mapMaybe (evalState (step tg) . initialState) . validVelocities $ tg
  where
    (Right tg) = runParser inputParser "" input

solverPartTwo :: String -> Int
solverPartTwo input = length . mapMaybe (evalState (step tg) . initialState) . validVelocities $ tg
  where
    (Right tg) = runParser inputParser "" input


day17 :: IO ()
day17 = do
  input <- readFile "src/Data/Day17.txt"
  putStrLn "Day 17"
  putStr "Part 1: "
  print . solverPartOne $ input
  putStr "Part 2: "
  print . solverPartTwo $ input

