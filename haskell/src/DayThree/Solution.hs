module DayThree.Solution (getIntersectionPrioritySum, main, mainWithFile) where

import Data.Map qualified as Map
import Data.Set qualified as Set
import PyF

splitInHalf :: String -> (String, String)
splitInHalf strToSplit = splitAt (div (length strToSplit + 1) 2) strToSplit

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)

getCharPriority :: Char -> Either String Integer
getCharPriority ch =
  let priorityMap = Map.fromList $ zip ['a' .. 'z'] [1 .. 26] ++ zip ['A' .. 'Z'] [27 .. 52]
      errorMsg = [fmt|Error, could not discern priority value for char: {ch}|]
   in maybeToEither errorMsg (Map.lookup ch priorityMap)

-- Exported for testing purposes
getIntersectionPrioritySum :: String -> Either String Integer
getIntersectionPrioritySum = (sum <$>) . mapM (Set.foldl accumPrioritiesOfCommonElems (return 0) . filterOutCommonElements . splitInHalf) . words
  where
    accumPrioritiesOfCommonElems acc v = let charPriority = getCharPriority v in (+) <$> charPriority <*> acc
    filterOutCommonElements = uncurry Set.intersection . mapTuple Set.fromList

main :: String -> IO ()
main inpStr = let output = either id show (getIntersectionPrioritySum inpStr) in print [fmt|Result: {output}|]

mainWithFile :: String -> IO ()
mainWithFile fileName = readFile fileName >>= main
