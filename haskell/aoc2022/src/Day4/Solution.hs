module Day4.Solution (
    main,
    mainWithFile,
    defaultInputFile,
    -- Exported for testing only
    computeSolutionOne,
    computeSolutionTwo,
) where

-- https://adventofcode.com/2022/day/4

import Data.Set (Set)
import Data.Text (Text)

import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as ML

import Control.Monad (guard)
import Data.Bool (bool)
import Data.Maybe (mapMaybe)
import Data.Void
import Helpers (fanThrough)
import PyF (fmt)

type Parser = Parsec Void Text

type AssignmentRange = Set Int
type AssignmentRangePair = (AssignmentRange, AssignmentRange)
type AssignmentRangePairs = [AssignmentRangePair]

parseInput :: Text -> AssignmentRangePairs
parseInput = mapMaybe (parseMaybe assignmentRangePairParser) . T.lines

assignmentRangePairParser :: Parser AssignmentRangePair
assignmentRangePairParser = (,) <$> assignmentRangeParser <*> (char ',' *> assignmentRangeParser) <* eof
  where
    assignmentRangeParser :: Parser AssignmentRange
    assignmentRangeParser = do
        lowerBound <- ML.decimal
        upperBound <- char '-' *> ML.decimal
        guard (upperBound >= lowerBound)
        pure $ toSet (lowerBound, upperBound)

    toSet :: (Int, Int) -> Set Int
    toSet (lowerBound, upperBound) = Set.fromList [lowerBound .. upperBound]

countValidAssignmentRangePairs :: (AssignmentRangePair -> Bool) -> AssignmentRangePairs -> Int
countValidAssignmentRangePairs predFn = foldr (bool id (+ 1) . predFn) 0

computeSolutionOne :: Text -> Int
computeSolutionOne = countValidAssignmentRangePairs isCompletelyOverlappingAssignmentRangePair . parseInput
  where
    isCompletelyOverlappingAssignmentRangePair (fstPair, sndPair) = fstPair `Set.isSubsetOf` sndPair || sndPair `Set.isSubsetOf` fstPair

computeSolutionTwo :: Text -> Int
computeSolutionTwo = countValidAssignmentRangePairs isPartiallyOverlappingAssignmentRangePair . parseInput
  where
    isPartiallyOverlappingAssignmentRangePair = not . uncurry Set.disjoint

main :: Text -> IO ()
main = putStrLn . formatAns . ((computeSolutionOne, computeSolutionTwo) `fanThrough`)
  where
    formatAns (solutionOne, solutionTwo) = [fmt|Solution one: {solutionOne}. Solution Two: {solutionTwo}|]

mainWithFile :: String -> IO ()
mainWithFile fileName = TIO.readFile fileName >>= main

defaultInputFile :: String
defaultInputFile = "./src/Day4/input.txt"
