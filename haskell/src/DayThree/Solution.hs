module DayThree.Solution (getIntersectionPrioritySum, main, mainWithFile) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import PyF

splitHalf :: String -> (String, String)
splitHalf strToSplit = splitAt (div (length strToSplit + 1) 2) strToSplit

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

getPriority :: Char -> Either String Integer
getPriority ch =
  let priorityMap = Map.fromList $ zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]
      errorMsg = [fmt|Error, could not discern priority value for char: {ch}|]
   in maybeToEither errorMsg (Map.lookup ch priorityMap)

-- Exported for testing purposes
getIntersectionPrioritySum :: String -> Either String Integer
getIntersectionPrioritySum = (sum <$>) . mapM (Set.foldl accumPrioritiesOfCommonElems (return 0) . uncurry Set.intersection . mapTuple Set.fromList . splitHalf) . words
  where
    accumPrioritiesOfCommonElems acc v = let charEither = getPriority v in (+) <$> charEither <*> acc

main :: String -> IO ()
main inpStr = let output = either id show (getIntersectionPrioritySum inpStr) in print [fmt|Result: {output}|]

mainWithFile :: String -> IO ()
mainWithFile fileName = readFile fileName >>= main
