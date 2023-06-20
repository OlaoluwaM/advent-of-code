{-# LANGUAGE ScopedTypeVariables #-}

module DayOneTest where

import Data.List (intercalate)
import DayOne.Solution
import Test.Tasty
import Test.Tasty.HUnit

test1 :: TestTree
test1 = testCase "Should ensure that we can discern the caloric total of the Elf that has the most calories" $ calculateMaxCalories input @?= expected
  where
    input = joinWithEmptyLine ["400\n500\n300", "200\n300", "700"]
    expected = 1200

test2 :: TestTree
test2 = testCase "Should ensure that we can discern the caloric total of the Elf that has the most calories 2" $ calculateMaxCalories input @?= expected
  where
    input = joinWithEmptyLine ["7000000", "800\n500\n10000", "20\n100\n665", "800"]
    expected = 7000000

test3 :: TestTree
test3 = testCase "Should ensure that calculateMaxCalories function output 0 on empty input" $ calculateMaxCalories input @?= expected
  where
    input = ""
    expected = 0

test4 :: TestTree
test4 = testCase "Should ensure that calculateMaxCalories function output 0 if input contains only new lines" $ calculateMaxCalories input @?= expected
  where
    input = joinWithEmptyLine ["\n", "\n", "\n"]
    expected = 0

test5 :: TestTree
test5 = testCase "Should ensure that calculateMaxCalories function skips non-integers in calculation" $ calculateMaxCalories input @?= expected
  where
    input = joinWithEmptyLine ["aString\n4000\nfdf\n800", "200\n300\n400", "400\nyyr\n900"]
    expected = 4800

test_maxCaloriesCalcFunction :: TestTree
test_maxCaloriesCalcFunction =
  testGroup
    "Tests for function used to calculate max calories"
    [test1, test2, test3, test4, test5]

-- Helpers

joinWithEmptyLine :: [String] -> String
joinWithEmptyLine = intercalate "\n\n"
