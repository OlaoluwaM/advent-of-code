module Utils where

import Data.List (intercalate)

joinWithEmptyLine :: [String] -> String
joinWithEmptyLine = intercalate "\n\n"

joinWithNewLine :: [String] -> String
joinWithNewLine = intercalate "\n"
