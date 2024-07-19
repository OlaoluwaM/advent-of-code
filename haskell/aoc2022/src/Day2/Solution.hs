-- Problem: https://adventofcode.com/2022/day/2

module Day2.Solution (
    main,
    mainWithFile,
    defaultInputFile,
    -- Exported for testing
    runRPSPart1,
    runRPSPart2,
) where

import Helpers
import PyF

import Data.Text (Text)

import Data.Text qualified as T
import Data.Text.IO qualified as T

import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- {-
--   Domain Rules

--   A -> Rock
--   B -> Paper
--   C -> Scissors

--   Rock -> 1pt (X)
--   Paper -> 2pt (Y)
--   Scissors -> 3pt (Z)

--   Win -> 6pt (Z)
--   Lose -> 0pt (X)
--   Draw -> 3pt (Y)
-- -}

data RPS = Rock | Paper | Scissors deriving (Eq, Show)

data Col1Input = A | B | C deriving (Eq, Show, Read)
data Col2Input = X | Y | Z deriving (Eq, Show, Read)

newtype OpponentOption = OpponentOption {play :: RPS} deriving (Eq, Show)
newtype PlayerOption = PlayerOption {play :: RPS} deriving (Eq, Show)

data RoundResult = Win | Lose | Draw deriving (Eq, Show, Read)

data Round = Round
    { playerOption :: PlayerOption
    , roundResult :: RoundResult
    , opponentOption :: OpponentOption
    }
    deriving (Eq, Show)

type Game = [Round]

type RoundInfo = (OpponentOption, Col2Input)

toOpponentOption :: Col1Input -> OpponentOption
toOpponentOption A = OpponentOption Rock
toOpponentOption B = OpponentOption Paper
toOpponentOption C = OpponentOption Scissors

parseInput :: Text -> [RoundInfo]
parseInput = mapMaybe (parseRound . T.unpack) . T.lines
  where
    parseRound [col1Input, _, col2Input] = first toOpponentOption <$> readMaybe @(Col1Input, Col2Input) [fmt|({col1Input},{col2Input})|]
    parseRound _ = Nothing

toGameForPart1 :: [RoundInfo] -> Game
toGameForPart1 =
    map
        ( \(opponentOption, col2Input) ->
            let playerOption = toPlayerOption col2Input
             in Round{roundResult = determineRoundResult (opponentOption, playerOption), playerOption = playerOption, opponentOption = opponentOption}
        )
  where
    determineRoundResult :: (OpponentOption, PlayerOption) -> RoundResult
    determineRoundResult (OpponentOption Rock, PlayerOption Paper) = Win
    determineRoundResult (OpponentOption Paper, PlayerOption Scissors) = Win
    determineRoundResult (OpponentOption Scissors, PlayerOption Rock) = Win
    determineRoundResult (OpponentOption Rock, PlayerOption Rock) = Draw
    determineRoundResult (OpponentOption Paper, PlayerOption Paper) = Draw
    determineRoundResult (OpponentOption Scissors, PlayerOption Scissors) = Draw
    determineRoundResult _ = Lose

    toPlayerOption :: Col2Input -> PlayerOption
    toPlayerOption X = PlayerOption Rock
    toPlayerOption Y = PlayerOption Paper
    toPlayerOption Z = PlayerOption Scissors

toGameForPart2 :: [RoundInfo] -> Game
toGameForPart2 =
    map
        ( \(opponentOption, col2Input) ->
            let desiredRoundResult = toRoundResult col2Input
             in Round{roundResult = desiredRoundResult, playerOption = determinePlayerOptionForRoundResult (opponentOption, desiredRoundResult), opponentOption = opponentOption}
        )
  where
    toRoundResult :: Col2Input -> RoundResult
    toRoundResult X = Lose
    toRoundResult Y = Draw
    toRoundResult Z = Win

    determinePlayerOptionForRoundResult :: (OpponentOption, RoundResult) -> PlayerOption
    determinePlayerOptionForRoundResult (OpponentOption Rock, Draw) = PlayerOption Rock
    determinePlayerOptionForRoundResult (OpponentOption Paper, Draw) = PlayerOption Paper
    determinePlayerOptionForRoundResult (OpponentOption Scissors, Draw) = PlayerOption Scissors
    determinePlayerOptionForRoundResult (OpponentOption Rock, Win) = PlayerOption Paper
    determinePlayerOptionForRoundResult (OpponentOption Paper, Win) = PlayerOption Scissors
    determinePlayerOptionForRoundResult (OpponentOption Scissors, Win) = PlayerOption Rock
    determinePlayerOptionForRoundResult (OpponentOption Rock, Lose) = PlayerOption Scissors
    determinePlayerOptionForRoundResult (OpponentOption Paper, Lose) = PlayerOption Rock
    determinePlayerOptionForRoundResult (OpponentOption Scissors, Lose) = PlayerOption Paper

calcTotalScore :: Game -> Integer
calcTotalScore = foldr (\(Round playerOption roundResult _) accumScore -> accumScore + determinePointsForPlay playerOption + determinePointsForRoundResult roundResult) 0
  where
    determinePointsForPlay :: PlayerOption -> Integer
    determinePointsForPlay (PlayerOption Rock) = 1
    determinePointsForPlay (PlayerOption Paper) = 2
    determinePointsForPlay (PlayerOption Scissors) = 3

    determinePointsForRoundResult :: RoundResult -> Integer
    determinePointsForRoundResult Win = 6
    determinePointsForRoundResult Draw = 3
    determinePointsForRoundResult Lose = 0

runRPSPart1 :: Text -> Integer
runRPSPart1 = calcTotalScore . toGameForPart1 . parseInput

runRPSPart2 :: Text -> Integer
runRPSPart2 = calcTotalScore . toGameForPart2 . parseInput

main :: Text -> IO ()
main txt = putStrLn $ formatAns (both calcTotalScore . (toGameForPart1 &&& toGameForPart2) . parseInput $ txt)
  where
    formatAns (totalScore1, totalScore2) =
        [fmt|\
{totalScore1}
{totalScore2}\
        |]

mainWithFile :: String -> IO ()
mainWithFile fileName = T.readFile fileName >>= main

defaultInputFile :: String
defaultInputFile = "./src/Day2/input.txt"
