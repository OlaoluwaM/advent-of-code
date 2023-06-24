module Main where

import DayOne.Solution qualified as Day1
import DayThree.Solution qualified as Day3
import DayTwo.SolutionAlt qualified as Day2
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO.Error (catchIOError, ioeGetErrorString)
import Text.Read (readMaybe)

type SolutionFns = (String -> IO (), String -> IO ())

main :: IO ()
main =
  getArgs >>= \case
    dayStr : args
      | Just day <- readMaybe dayStr -> runDaySolution day args
    _ -> error "USAGE: haskell-exe DAY ARGS..."

runDaySolution :: Int -> [String] -> IO ()
runDaySolution day args = case day of
  1 -> executeWithFileOrStdin args (Day1.main, Day1.mainWithFile)
  2 -> executeWithFileOrStdin args (Day2.main, Day2.mainWithFile)
  3 -> executeWithFileOrStdin args (Day3.main, Day3.mainWithFile)
  x -> error $ unwords ["Day not completed", show x]

executeWithFileOrStdin :: [String] -> SolutionFns -> IO ()
executeWithFileOrStdin args (ioWithStr, ioWithFile)
  | null args = do
      putStrLn "No file supplied. Enter your inputs..."
      stdin' <- getContents
      ioWithStr stdin'
  | otherwise = do
      let file = head args
      let handler e = putStrLn $ ioeGetErrorString e
      let getResult = catchIOError (doesFileExist file >> ioWithFile file) handler

      putStrLn $ "Reading file: " ++ file ++ "..."
      getResult
