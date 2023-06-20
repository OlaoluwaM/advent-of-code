module DayOne.Solution where

-- Problem: https://adventofcode.com/2022/day/1

import Data.List.Split (splitWhen)
import Data.Maybe
import Text.Read (readMaybe)

type ParsedInput = [[Integer]]

getMaxCalories :: ParsedInput -> Integer
getMaxCalories = maximum . map sum

parseInput :: String -> ParsedInput
parseInput = map toSafeInteger . splitOnEmptyString . lines
  where
    toSafeInteger = map (fromMaybe 0 . safeIntegerRead)
    splitOnEmptyString = splitWhen (== "")

safeIntegerRead :: String -> Maybe Integer
safeIntegerRead = readMaybe

main :: IO ()
main = do
  inputFileContents <- readFile "input.txt"
  print $ getMaxCalories $ parseInput inputFileContents
