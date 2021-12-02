module AdventOfCode.Day1 where
import GHC.IO.Buffer (slideContents)

day1 :: IO ()
day1 = do
    input <- readFile "src/Data/Day1.txt"
    print . solverPartOne $ input
    print . solverPartTwo $ input

parseInput :: String -> [Int]
parseInput = map read . lines

diffs :: [Int] -> [Int]
diffs report = zipWith (-) (tail report) report

solverPartOne :: String -> Int
solverPartOne = length . filter (>0) . diffs . parseInput

slidingWindow :: Int -> [Int] -> [[Int]]
slidingWindow n ls
    | length ls == n = [ls]
    | length ls  > n = take n ls : slidingWindow n (tail ls)
    | otherwise      = []


solverPartTwo :: String -> Int
solverPartTwo = length . filter (>0) . diffs . map sum . slidingWindow 3 . parseInput