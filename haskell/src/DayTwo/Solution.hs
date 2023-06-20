{-# LANGUAGE LambdaCase #-}
module DayTwo.Solution where

import Data.Bifunctor (first)
import Text.Read (readEither)

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

data OpponentPlayOption = A | B | C deriving (Show, Eq, Read)

data PlayOption = X | Y | Z deriving (Show, Eq, Read)

data Round = OpponentPlayOption `Vs` PlayOption

data RoundResult = Loss | Draw | Win deriving (Show)

headToHeadScore :: Round -> Integer
headToHeadScore = \case
  A `Vs` Y -> win
  B `Vs` Z -> win
  C `Vs` X -> win
  A `Vs` X -> draw
  B `Vs` Y -> draw
  C `Vs` Z -> draw
  _ -> lose
  where
    win = 6
    draw = 3
    lose = 0

playScore :: Round -> Integer
playScore (_ `Vs` p) = case p of
  X -> 1
  Y -> 2
  Z -> 3

roundScore :: Round -> Integer
roundScore r = headToHeadScore r + playScore r

parseRounds :: String -> Either String [Round]
parseRounds = traverse (uncurry parseRoundNumber) . zip [1..] . lines

parseRoundNumber :: Int -> String -> Either String Round
parseRoundNumber n =
  first ((unwords ["Error in round", show n]) ++) . parseRound

parseRound :: String -> Either String Round
parseRound = \case
  [x, ' ', y] ->
    Vs
    <$> first ("Error parsing OpponentPlayOption" ++) (readEither [x])
    <*> first ("Error parsing PlayOption" ++) (readEither [y])
  invalid -> Left $ unwords ["Invalid round: \"", invalid, " \""]

runRound :: String -> Either String Integer
runRound = fmap (sum . fmap roundScore) . parseRounds

main :: IO ()
main = readFile "./input.txt" >>= pure . runRound >>= either error print
