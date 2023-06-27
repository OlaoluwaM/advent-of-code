-- ---------------------------------------------- --
--                 Advent of Code                 --
--               Day 4: Camp Cleanup              --
--       https://adventofcode.com/2022/day/4      --
-- ---------------------------------------------- --
module DayFour.Solution (solution, main, mainWithFile) where

import Data.List.Split (splitOn)
import Helpers (mapTuple)
import Text.Read (readMaybe)

import GHC.Base (liftM2)
import PyF

{-
  Solution

  A range of IDs A is a complete subset of another range of IDs B if:
    the upper bound of B is greater than or equal to the upper bound of A
    the lower bound of B is less than or equal to the lower bound of A

  A: 3 - 6
  B: 2 - 7

  The upper bound of B, 7, is greater than the upper bound of A (6), and the lower bound of B is less than that of A. Thus, the range A is within B
-}

type IDRange = (Word, Word)
type IDRangePair = (IDRange, IDRange)

main :: String -> IO ()
main inpStr = let output = solution inpStr in print [fmt|Result. There are {output} complete overlaps|]

mainWithFile :: String -> IO ()
mainWithFile fileName = readFile fileName >>= main

-- ---------------------------------------------- --
--                    Solution                    --
-- ---------------------------------------------- --

solution :: String -> Int
solution = length . filter doesRangePairContainCompleteOverlap . map (sequenceTupleMaybe . mapTuple parseIdRange . toTuple2 "0-0" . splitOn ",") . lines

-- "90-95" -> Just (90, 95)
parseIdRange :: String -> Maybe IDRange
parseIdRange = (normalizeIdRange . toTuple2 (0 :: Word) <$>) . traverse readMaybe . splitOn "-"
 where
  normalizeIdRange (lB, uB)
    | lB > uB || uB < lB = (uB, lB)
    | uB == 0 = (lB, lB)
    | lB == 0 = (1, uB)
    | otherwise = (lB, uB)

doesRangePairContainCompleteOverlap :: Maybe IDRangePair -> Bool
doesRangePairContainCompleteOverlap = \case
  Just ((idRange1LowerB, idRange1UpperB), (idRange2LowerB, idRange2UpperB)) -> firstRangeIsWithinSecond || secondRangeIsWithinFirst
   where
    firstRangeIsWithinSecond = idRange2UpperB >= idRange1UpperB && idRange2LowerB <= idRange1LowerB
    secondRangeIsWithinFirst = idRange1UpperB >= idRange2UpperB && idRange1LowerB <= idRange2LowerB
  _ -> False

-- ---------------------------------------------- --
--                     Helpers                    --
-- ---------------------------------------------- --
toTuple2 :: a -> [a] -> (a, a)
toTuple2 def [x] = (x, def)
toTuple2 _ [x, y] = (x, y)
toTuple2 def _ = (def, def)

sequenceTupleMaybe :: (Maybe a, Maybe a) -> Maybe (a, a)
sequenceTupleMaybe = uncurry $ liftM2 (,)
