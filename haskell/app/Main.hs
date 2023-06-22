{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import DayOne.Solution qualified
import DayTwo.Solution qualified

main :: IO ()
main = getArgs >>= \case
  dayStr : args | Just day <- readMaybe dayStr -> runDay day args
  _ -> error "USAGE: haskell-exe DAY ARGS..."

runDay :: Int -> [String] -> IO ()
runDay day args = case day of
  1 -> DayOne.Solution.main args
  2 -> DayTwo.Solution.main args
  x -> error $ unwords ["Bad day", show x]
