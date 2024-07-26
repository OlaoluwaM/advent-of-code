module Helpers where

import Data.Text (Text)

import Data.Text qualified as T

import Data.Maybe
import Text.Read

import Control.Arrow (Arrow, (&&&))
import Data.Bifunctor (Bifunctor (bimap))
import Debug.Trace (trace)

splitInHalf :: Text -> (Text, Text)
splitInHalf s = T.splitAt (div (T.length s + 1) 2) s

splitInHalfS :: String -> (String, String)
splitInHalfS s = splitAt (div (length s + 1) 2) s

both :: (a -> b) -> (a, a) -> (b, b)
both f = bimap f f

toSafeInteger :: Text -> Integer
toSafeInteger = fromMaybe 0 . readMaybe . T.unpack

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

fanThrough :: (Arrow a) => (a b c, a b c') -> a b (c, c')
fanThrough = uncurry (&&&)

headF :: (Foldable f) => f a -> Maybe a
headF = foldr (\x _ -> pure x) Nothing

myTrace :: (Show a) => String -> a -> a
myTrace str' a = trace (str' <> show a) a
