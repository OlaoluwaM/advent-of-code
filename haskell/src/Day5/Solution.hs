-- ---------------------------------------------- --
--                 Advent of Code                 --
--               Day 5: Supply Stacks             --
--       https://adventofcode.com/2022/day/5      --
-- ---------------------------------------------- --
module Day5.Solution where

import Control.Monad.State (State, execState, get, modify, put)
import Data.List (foldl', isPrefixOf, transpose, intercalate)
import Data.List.Split (linesBy, splitOn, splitOneOf, splitWhen, startsWith)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Text.Read (readMaybe)

import Control.Lens (element, set)
import PyF

type RawCargoLayout = [[String]]
type RawCraneInstruction = String

data CraneInstruction = Move
  { noOfCrates :: Int
  , fromIx :: Int
  , toIx :: Int
  }
  deriving (Show)

-- instance Show CraneInstruction where
--   show (Move noOfCrates' fromIx' toIx') = "Move "

type PartiallyParsedInputs = (RawCargoLayout, [RawCraneInstruction])
type CargoLayout = [Stack String ()]
type CraneOperation = (CargoLayout -> CargoLayout)
type ParsedInput = (CargoLayout, [CraneOperation])

main :: IO ()
main = readFile "./input.txt" >>= print . fst . mapBoth (map lines) (drop 1) . break (== "") . lines

foo :: ParsedInput -> CargoLayout
foo (x, y) = foldl' (\acc f -> f acc) x (take 1 y)

parseInput :: String -> ParsedInput
parseInput = mapBoth parseIntoCargoLayout (map baz . removeAllInvalidCraneInstructions . map toInputCraneInstructions) . partiallyParseInput
 where
  partiallyParseInput = mapBoth (transpose . map words . removeLastElem) (drop 1) . break (== "") . lines
  removeAllInvalidCraneInstructions = fromMaybe [] . sequenceFilterMaybe

baz :: CraneInstruction -> (CargoLayout -> CargoLayout)
baz (Move noOfCrates' fromIx' toIx') cargoLayout = do
  let crateStackToRemoveFrom = cargoLayout !! fromIx'
  let crateStackToAddTo = cargoLayout !! toIx'
  let crateStackAfterRemove = crateStackToRemoveFrom >> pop noOfCrates'
  let crateStackAfterAddition = crateStackAfterRemove >>= \removedCrates -> crateStackToAddTo >> push removedCrates
  let ccc = crateStackAfterRemove >> return ()
  -- let v = set (element fromIx') ccc cargoLayout
  set (element toIx') crateStackAfterAddition $ set (element fromIx') ccc cargoLayout

parseIntoCargoLayout :: RawCargoLayout -> CargoLayout
parseIntoCargoLayout = map put

toInputCraneInstructions :: RawCraneInstruction -> Maybe CraneInstruction
toInputCraneInstructions rawCraneI = case words rawCraneI of
  ["move", noOfCrates', "from", fromIx', "to", toIx'] -> traverse readInteger [noOfCrates', fromIx', toIx'] >>= foldIntoCraneInstructions
  _ -> Nothing

foldIntoCraneInstructions :: [Int] -> Maybe CraneInstruction
foldIntoCraneInstructions [noOfCrates', fromIx', toIx'] = Just $ Move noOfCrates' (fromIx' - 1) (toIx' - 1)
foldIntoCraneInstructions _ = Nothing

-- ---------------------------------------------- --
--              Stack Implementation              --
-- ---------------------------------------------- --

type Stack a b = State [a] b

push :: [String] -> Stack String ()
push newCrate = modify (newCrate ++)

pop :: Int -> Stack String [String]
pop removeCount = do
  crates <- get
  let remainingCrates = drop removeCount crates
  let removedCrates = take removeCount crates
  put remainingCrates
  return removedCrates

-- ---------------------------------------------- --
--                     Helpers                    --
-- ---------------------------------------------- --

toTuple :: [String] -> ([String], [String])
toTuple [x, y] = ([x], [y])
toTuple _ = ([""], [""])

mapFst :: (a -> b) -> (a, a) -> (b, a)
mapFst f (x, y) = (f x, y)

mapSnd :: (a -> b) -> (a, a) -> (a, b)
mapSnd f (x, y) = (x, f y)

mapBoth :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapBoth f g (x, y) = (f x, g y)

removeLastElem :: [a] -> [a]
removeLastElem lst = take (length lst - 1) lst

readInteger :: String -> Maybe Int
readInteger = readMaybe

sequenceFilterMaybe :: [Maybe a] -> Maybe [a]
sequenceFilterMaybe mas = sequence $ filter isJust mas

-- import Data.List (transpose)

parseInput'' :: String -> [[String]]
parseInput'' = transpose . map (splitOn " " . intercalate " " . words) . lines

input =
  "[Q] [J]                         [H]\n\
  \[G] [S] [Q]     [Z]             [P]\n\
  \[P] [F] [M]     [F]     [F]     [S]\n\
  \[R] [R] [P] [F] [V]     [D]     [L]\n\
  \[L] [W] [W] [D] [W] [S] [V]     [G]\n\
  \[C] [H] [H] [T] [D] [L] [M] [B] [B]\n\
  \[T] [Q] [B] [S] [L] [C] [B] [J] [N]\n\
  \[F] [N] [F] [V] [Q] [Z] [Z] [T] [Q]\n\
  \ 1   2   3   4   5   6   7   8   9"

-- main = print $ parseInput input
