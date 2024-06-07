module Day1.Solution (
    main,
    mainWithFile,
    defaultInputFile,
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

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T

type CaloriesInput = Text
type ParsedInput = [Integer]

parseInput :: Text -> ParsedInput
parseInput = List.sortOn Down . map (sum . map toSafeInteger) . splitWhen (== "") . T.lines

calcMaxCaloriesOverall :: CaloriesInput -> Integer
calcMaxCaloriesOverall = fromMaybe 0 . listToMaybe . parseInput

calcTopThreeCaloriesAsSum :: CaloriesInput -> Integer
calcTopThreeCaloriesAsSum = sum . take 3 . parseInput

main :: Text -> IO ()
main txt = putStrLn $ formatAns (calcMaxCaloriesOverall txt, calcTopThreeCaloriesAsSum txt)
  where
    formatAns (generalMaxCalories, topThreeMaxCalorieSum) =
        [fmt|\
{generalMaxCalories}
{topThreeMaxCalorieSum}\
        |]

mainWithFile :: String -> IO ()
mainWithFile fileName = T.readFile fileName >>= main

defaultInputFile :: String
defaultInputFile = "./src/Day1/input.txt"
