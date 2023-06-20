module DayTwo.Solution where

import Data.Maybe (fromMaybe)
import Text.Read

-- Problem: https://adventofcode.com/2022/day/2

{-
  A -> Rock (1pt)
  B -> Paper (2pt)
  C -> Scissors (3pt)

  X -> Rock
  Y -> Paper
  Z -> Scissors

  N -> No op in cases where parsing fail (0pt)

  Win -> 6pt
  Lose -> 0pt
  Draw -> 3pt
-}

data OpponentPlayOptions = A | B | C | NopPlay deriving (Show, Eq, Read)

data PlayOptions = X | Y | Z | Nplay deriving (Show, Eq, Read)

type RoundPlay = (OpponentPlayOptions, PlayOptions)

type RoundPlays = [RoundPlay]

type RawRoundPlays = String

type RoundOutcome = (PlayOptions, Integer)

calculateTotalPoints :: RawRoundPlays -> Integer
calculateTotalPoints rawRoundPlays = sum [sumTuple $ getTotalRoundOutcomePts roundPlay playOption | roundPlay@(_, playOption) <- roundPlays]
  where
    sumTuple (x, y) = x + y
    roundPlays = fromRawToParsedRoundPlays rawRoundPlays
    getTotalRoundOutcomePts roundPlay playOption = (parsePlayOptionIntoPoints playOption, parseRoundPlayIntoOutcomePoints roundPlay)

-- CHange name
parseRoundPlayIntoOutcomePoints :: RoundPlay -> Integer
parseRoundPlayIntoOutcomePoints roundPlay = case roundPlay of
  (A, Y) -> win
  (B, Z) -> win
  (C, X) -> win
  (NopPlay, _) -> win
  (A, X) -> draw
  (B, Y) -> draw
  (C, Z) -> draw
  (_, _) -> lose
  where
    win = 6
    draw = 3
    lose = 0

parsePlayOptionIntoPoints :: PlayOptions -> Integer
parsePlayOptionIntoPoints X = 1
parsePlayOptionIntoPoints Y = 2
parsePlayOptionIntoPoints Z = 3
parsePlayOptionIntoPoints _ = 0

fromRawToParsedRoundPlays :: RawRoundPlays -> RoundPlays
fromRawToParsedRoundPlays rawSG = [(safeOpponentPlayOptionsParse [x], safePlayOptionsParse [y]) | [x, _, y] <- lines rawSG]

safeOpponentPlayOptionsParse :: String -> OpponentPlayOptions
safeOpponentPlayOptionsParse = fromMaybe NopPlay . readMaybe

safePlayOptionsParse :: String -> PlayOptions
safePlayOptionsParse = fromMaybe Nplay . readMaybe

-- main :: IO ()
main :: IO ()
main = readFile "./input.txt" >>= print . calculateTotalPoints
