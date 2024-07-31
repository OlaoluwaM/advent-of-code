module Main where

import Data.Text (Text)
import Data.Vector (Vector)

import Data.Text qualified as T
import Data.Text.IO qualified as T

import Data.Vector qualified as V
import Day1.Solution qualified as Day1
import Day2.Solution qualified as Day2
import Day3.Solution qualified as Day3
import Day4.Solution qualified as Day4

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO.Error (catchIOError, ioeGetErrorString)
import Text.Read (readMaybe)

type SolutionFns = (Text -> IO (), String -> IO ())

defaultInputFilePaths :: Vector String
defaultInputFilePaths = V.fromList [Day1.defaultInputFile, Day2.defaultInputFile, Day3.defaultInputFile, Day4.defaultInputFile]

main :: IO ()
main =
    getArgs >>= \case
        dayStr : args
            | Just day <- readMaybe dayStr -> runDaySolution day (T.pack <$> args)
        _ -> error "USAGE: haskell-exe DAY ARGS..."

runDaySolution :: Int -> [Text] -> IO ()
runDaySolution day args = case day of
    1 -> executeWithFileOrStdin day args (Day1.main, Day1.mainWithFile)
    2 -> executeWithFileOrStdin day args (Day2.main, Day2.mainWithFile)
    3 -> executeWithFileOrStdin day args (Day3.main . T.unpack, Day3.mainWithFile)
    4 -> executeWithFileOrStdin day args (Day4.main, Day4.mainWithFile)
    _ -> error $ "There is no solution for day " <> show day <> " yet"

executeWithFileOrStdin :: Int -> [Text] -> SolutionFns -> IO ()
executeWithFileOrStdin day args (ioWithTxt, ioWithFile)
    | null args = do
        putStrLn "No custom input file supplied. Using default input file instead..."
        maybe
            (putStrLn $ "Couldn't find a default input file for your day " <> show day <> " solution")
            ioWithFile
            (defaultInputFilePaths V.!? (day - 1))

        putStrLn ""
        putStrLn "Now reading from stdin..."
        stdin' <- T.getContents

        putStrLn ""
        putStrLn "Your results are: "
        ioWithTxt stdin'
    | otherwise = do
        let file = T.unpack $ head args
        let handler e = putStrLn $ ioeGetErrorString e

        putStrLn $ "Reading file: " <> file <> "..."
        catchIOError (doesFileExist file >> ioWithFile file) handler
