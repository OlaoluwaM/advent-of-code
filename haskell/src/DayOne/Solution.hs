module DayOne.Solution (calculateMaxCalories) where

-- Problem: https://adventofcode.com/2022/day/1

import Data.Maybe
import Text.Read (readMaybe)

type ParsedInput = [[Integer]]

type CaloriesInput = String

getMaxCalories :: ParsedInput -> Integer
getMaxCalories = maximum . map sum

parseInput :: CaloriesInput -> ParsedInput
parseInput = map toSafeInteger . chunkUpToEmptyString . lines
  where
    toSafeInteger = map (fromMaybe 0 . safeIntegerRead)

-- Attribution: https://github.com/Ma-Fi-94/advent-of-haskell-22/blob/main/day1/solution.hs
chunkUpToEmptyString :: [String] -> [[String]]
chunkUpToEmptyString strings = chunkUpToEmptyString' strings [] []
  where
    chunkUpToEmptyString' [] groupsList currentGroup = groupsList ++ [currentGroup]
    chunkUpToEmptyString' [""] groupsList currentGroup = groupsList ++ [currentGroup]
    chunkUpToEmptyString' ("" : xs) groupsList currentGroup = chunkUpToEmptyString' xs (groupsList ++ [currentGroup]) []
    chunkUpToEmptyString' (x : xs) groupsList currentGroup = chunkUpToEmptyString' xs groupsList (currentGroup ++ [x])

safeIntegerRead :: CaloriesInput -> Maybe Integer
safeIntegerRead = readMaybe

calculateMaxCalories :: CaloriesInput -> Integer
calculateMaxCalories = getMaxCalories . parseInput

-- main :: IO ()
-- main = do
--   inputFileContents <- readFile "input.txt"
--   print $ calculateMaxCalories inputFileContents
