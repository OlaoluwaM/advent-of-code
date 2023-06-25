----------------------------------------------------
--                                                --
--              Advent of Code 2022               --
--           Day 2: Rock Paper Scissors           --
--       https://adventofcode.com/2022/day/2      --
--    Courtesy of: https://github.com/ramirez7    --
--                                                --
----------------------------------------------------
module DayTwo.SolutionAlt (runRPS, main, mainWithFile) where

import Data.Bifunctor (first)
import PyF
import Text.Read (readEither)

{-
  Domain Rules

  A -> Rock (1pt)
  B -> Paper (2pt)
  C -> Scissors (3pt)

  X -> Rock
  Y -> Paper
  Z -> Scissors

  Win -> 6pt
  Lose -> 0pt
  Draw -> 3pt
-}

data OpponentPlayOption = A | B | C deriving (Show, Eq, Read)

data PlayOption = X | Y | Z deriving (Show, Eq, Read)

data Round = OpponentPlayOption `Vs` PlayOption

data RoundResult = Loss | Draw | Win deriving (Show)

runRound :: Round -> RoundResult
runRound = \case
  A `Vs` Y -> Win
  B `Vs` Z -> Win
  C `Vs` X -> Win
  A `Vs` X -> Draw
  B `Vs` Y -> Draw
  C `Vs` Z -> Draw
  _ -> Loss

resultScore :: RoundResult -> Integer
resultScore = \case
  Loss -> 0
  Draw -> 3
  Win -> 6

playScore :: Round -> Integer
playScore (_ `Vs` p) = case p of
  X -> 1
  Y -> 2
  Z -> 3

roundScore :: Round -> Integer
roundScore r = resultScore (runRound r) + playScore r

parseRounds :: String -> Either String [Round]
parseRounds = traverse (uncurry parseRoundNumber) . zip [1 ..] . lines

parseRoundNumber :: Int -> String -> Either String Round
parseRoundNumber n =
  first (unwords ["Error in round", show n, ": "] ++) . parseRound

parseRound :: String -> Either String Round
parseRound = \case
  [x, ' ', y] ->
    Vs
      <$> first ("Error parsing OpponentPlayOption: " ++) (readEither [x])
      <*> first ("Error parsing PlayOption: " ++) (readEither [y])
  invalid -> Left $ unwords ["Invalid round: \"", invalid, " \""]

-- Exported for testing purposes
runRPS :: String -> Either String Integer
runRPS = fmap (sum . fmap roundScore) . parseRounds

main :: String -> IO ()
main s = let output = either id show (runRPS s) in putStrLn [fmt|Result: {output}|]

mainWithFile :: String -> IO ()
mainWithFile fileName = readFile fileName >>= main
