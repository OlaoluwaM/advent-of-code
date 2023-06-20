module Spec where

import DayOne.Solution
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [unitTests1]

unitTests1 :: TestTree
unitTests1 =
  testGroup
    "Tests for the ff"
    [testForCorrectness]
  where
    testForCorrectness = testCase "Should ensure that we can discern the caloric total of the Elf that has the most calories" $ calculateMaxCalories "400\n\n300\n500" @?= 700
