module Day1.Solution (
    main,
    mainWithFile,
    -- Exported for testing
    calcMaxCaloriesOverall,
    calcTopThreeCaloriesAsSum,
) where

-- Problem: https://adventofcode.com/2022/day/1

import Data.List.Split (splitWhen)
import Data.Maybe
import PyF

import Data.List qualified as List
import Data.Ord (Down (Down))
import Helpers (toSafeInteger)

type CaloriesInput = String
type ParsedInput = [Integer]

parseInput :: String -> ParsedInput
parseInput = List.sortOn Down . map (sum . map toSafeInteger) . splitWhen (== "") . lines

calcMaxCaloriesOverall :: CaloriesInput -> Integer
calcMaxCaloriesOverall = fromMaybe 0 . listToMaybe . parseInput

calcTopThreeCaloriesAsSum :: CaloriesInput -> Integer
calcTopThreeCaloriesAsSum = sum . take 3 . parseInput

main :: String -> IO ()
main s = putStrLn $ formatAns (calcMaxCaloriesOverall s, calcTopThreeCaloriesAsSum s)
  where
    formatAns (generalMaxCalories, topThreeMaxCalorieSum) =
        [fmt|\
{generalMaxCalories}
{topThreeMaxCalorieSum}\
        |]

mainWithFile :: String -> IO ()
mainWithFile fileName = readFile fileName >>= main
