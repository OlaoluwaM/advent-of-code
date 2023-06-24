module Helpers where

splitInHalf :: String -> (String, String)
splitInHalf strToSplit = splitAt (div (length strToSplit + 1) 2) strToSplit

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)
