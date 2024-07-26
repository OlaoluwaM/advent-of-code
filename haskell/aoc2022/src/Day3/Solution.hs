-- Problem: https://adventofcode.com/2022/day/3
module Day3.Solution (
    main,
    mainWithFile,
    defaultInputFile,
    -- For Testing Purposes
    getSolutionRucksacks,
    getSolutionElfGroups,
) where

import Data.Set (Set)
import Data.Set qualified as Set

import Helpers (both, splitInHalfS)

import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)

import Control.Arrow ((&&&))
import PyF

-- Use the `lines` function to convert inp to list of strings
-- filter out elems whose length is not divisible by 2
-- Divide each rucksack into a tuple (a, b) where `a` reprsents the first compartment while b the other. We can make both sets and find the intersection
-- For all shared elems, get the priority
-- Sum up the priority

-- Each rucksack has exactly two compartments of the same size so the raw input must be of even length
-- Add a little explanation as to why we're using Char and String instead of Text
type Rucksack = (Set Char, Set Char)
type ElfGroup = NonEmpty (Set Char)

elfGroupSize :: Int
elfGroupSize = 3

isValidInputStr :: String -> Bool
isValidInputStr strng = isValidString strng && (even . length) strng
  where
    isValidString :: String -> Bool
    isValidString = foldr (\char -> (&& isLetter char)) True

parseRuckSacks :: String -> [Rucksack]
parseRuckSacks = map (both Set.fromList . splitInHalfS) . filter isValidInputStr . lines

parseElfGroups :: String -> [ElfGroup]
parseElfGroups = mapMaybe toElfGroup . filter isValidElfGroup . chunksOf elfGroupSize . lines
  where
    toElfGroup :: [String] -> Maybe ElfGroup
    toElfGroup rs
        | length rs == elfGroupSize = NE.fromList . fmap Set.fromList <$> Just rs
        | otherwise = Nothing

    isValidElfGroup :: [String] -> Bool
    isValidElfGroup = (== elfGroupSize) . length . filter isValidInputStr

getItemReorgPriorityForRucksacks :: [Rucksack] -> Int
getItemReorgPriorityForRucksacks = foldr ((+) . sum . getItemReorgPriorityForRucksack) 0
  where
    getItemReorgPriorityForRucksack :: Rucksack -> Set Int
    getItemReorgPriorityForRucksack = Set.map getItemReorgPriority . uncurry Set.intersection

getElfGroupBadgesReorgPriority :: [ElfGroup] -> Int
getElfGroupBadgesReorgPriority = foldr ((+) . sum . getElfGroupBadge) 0
  where
    getElfGroupBadge :: ElfGroup -> Set Int
    getElfGroupBadge = Set.map getItemReorgPriority . foldr1 Set.intersection

getItemReorgPriority :: Char -> Int
getItemReorgPriority char
    -- ord 'a' - 1 == 96
    -- ord 'a' - 96 == 1
    -- ord 'b' - 96 == 2
    -- ord 'c' - 96 == 3
    -- ...
    -- ord 'z' - 96 == 26
    | isLower char = let offset = ord 'a' - 1 in ord char - offset
    -- ord 'A' - 38 == 27
    -- ord 'B' - 38 == 28
    -- ord 'C' - 38 == 29
    -- ...
    -- ord 'Z' - 38 == 52
    | isUpper char = let offset = 38 in ord char - offset
    | otherwise = 0

getSolutionRucksacks :: String -> Int
getSolutionRucksacks = getItemReorgPriorityForRucksacks . parseRuckSacks

getSolutionElfGroups :: String -> Int
getSolutionElfGroups = getElfGroupBadgesReorgPriority . parseElfGroups

main :: String -> IO ()
main = putStrLn . formatAns . ((getElfGroupBadgesReorgPriority . parseElfGroups) &&& (getItemReorgPriorityForRucksacks . parseRuckSacks))
  where
    formatAns (ans1, ans2) =
        [fmt|\
{ans1}
{ans2}\
        |]

mainWithFile :: String -> IO ()
mainWithFile fileName = readFile fileName >>= main

defaultInputFile :: String
defaultInputFile = "./src/Day3/input.txt"
