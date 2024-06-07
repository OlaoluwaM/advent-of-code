-- ----------------------------------------------------
-- --                                                --
-- --              Advent of Code 2022               --
-- --         Day 3: Rucksack Reorganization         --
-- --       https://adventofcode.com/2022/day/3      --
-- --                                                --
-- ----------------------------------------------------
-- module Day3.Solution (part1Solution, main, mainWithFile) where
module Day3.Solution where

-- import Data.Map qualified as Map
-- import Data.Set qualified as Set
-- import Helpers
-- import PyF

-- import Control.Error (note)

-- main :: String -> IO ()
-- main inpStr = let output = either id show (part1Solution inpStr) in print [fmt|Result: {output}|]

-- mainWithFile :: String -> IO ()
-- mainWithFile fileName = readFile fileName >>= main

-- -- Solution

-- part1Solution :: String -> Either String Integer
-- part1Solution = (sum <$>) . mapM (amassPrioritiesOfCommonElements . filterOutCommonElements . splitSackContentsIntoCompartments) . words
--  where
--   amassPrioritiesOfCommonElements = Set.foldl sumPriorities (return 0)
--   sumPriorities acc v = let charPriority = getCharPriority v in (+) <$> charPriority <*> acc
--   filterOutCommonElements = uncurry Set.intersection . mapTuple Set.fromList
--   splitSackContentsIntoCompartments = splitInHalf

-- getCharPriority :: Char -> Either String Integer
-- getCharPriority ch =
--   let priorityMap = Map.fromList $ zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]
--       errorMsg = [fmt|Error, could not discern priority value for char: {ch}|]
--    in note errorMsg (Map.lookup ch priorityMap)
