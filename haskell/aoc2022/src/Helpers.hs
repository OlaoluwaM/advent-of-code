module Helpers where

import Data.Text (Text)

import Data.Text qualified as T

import Data.Maybe
import Text.Read

import Data.Bifunctor (Bifunctor (bimap))

splitInHalf :: Text -> (Text, Text)
splitInHalf s = T.splitAt (div (T.length s + 1) 2) s

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f = bimap f f

toSafeInteger :: Text -> Integer
toSafeInteger = fromMaybe 0 . readMaybe . T.unpack

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show
