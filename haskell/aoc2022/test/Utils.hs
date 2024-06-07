module Utils where

import Test.QuickCheck

import Data.Char
import Data.List (intercalate)

import Data.Text (Text)

import Data.Text qualified as T

joinWithEmptyLine :: [Text] -> Text
joinWithEmptyLine = T.intercalate "\n\n"

joinWithNewLine :: [Text] -> Text
joinWithNewLine = T.intercalate "\n"

newtype AlphaChar = AlphaChar {unAlphaChar :: Char} deriving (Eq, Show)

genAlphaChar :: Gen AlphaChar
genAlphaChar = do
    let upperCaseAlphaRange = (ord 'A', ord 'Z')
    let lowerCaseAlphaRange = (ord 'a', ord 'z')
    charCode <- oneof [chooseInt upperCaseAlphaRange, chooseInt lowerCaseAlphaRange]

    pure $ AlphaChar $ chr charCode

instance Arbitrary AlphaChar where
    arbitrary = genAlphaChar
