module Solutions.DayOneTest where

import Data.List (intercalate)
import DayOne.Solution (calculateMaxCalories)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Tasty
import Test.Tasty.Hedgehog
import Utils

-- ---------------------------------------------- --
--                   Unit Tests                   --
-- ---------------------------------------------- --
spec_solution :: Spec
spec_solution = do
  describe "AOC Day 1: Unit Tests" $ do
    describe "Tests on valid inputs" $ do
      correctInputUnitTest1
      correctInputUnitTest2
    describe "Tests on invalid inputs" $ do
      incorrectInputUnitTest1
      incorrectInputUnitTest2
      incorrectInputUnitTest3

correctInputUnitTest1 :: SpecWith ()
correctInputUnitTest1 = do
  it "Should ensure that we can discern the caloric total of the Elf that has the most calories" $ calculateMaxCalories input `shouldBe` expected
  where
    input = joinWithEmptyLine ["400\n500\n300", "200\n300", "700"]
    expected = 1200

correctInputUnitTest2 :: SpecWith ()
correctInputUnitTest2 = do
  it "Should ensure that we can discern the caloric total of the Elf that has the most calories 2" $ calculateMaxCalories input `shouldBe` expected
  where
    input = joinWithEmptyLine ["7000000", "800\n500\n10000", "20\n100\n665", "800"]
    expected = 7000000

incorrectInputUnitTest1 :: SpecWith ()
incorrectInputUnitTest1 = do
  it "Should ensure that calculateMaxCalories function output 0 on empty input" $ calculateMaxCalories input `shouldBe` expected
  where
    input = ""
    expected = 0

incorrectInputUnitTest2 :: SpecWith ()
incorrectInputUnitTest2 = do
  it "Should ensure that calculateMaxCalories function output 0 if input contains only new lines" $ calculateMaxCalories input `shouldBe` expected
  where
    input = joinWithEmptyLine ["\n", "\n", "\n"]
    expected = 0

incorrectInputUnitTest3 :: SpecWith ()
incorrectInputUnitTest3 = do
  it "Should ensure that calculateMaxCalories function skips non-integers in calculation" $ calculateMaxCalories input `shouldBe` expected
  where
    input = joinWithEmptyLine ["aString\n4000\nfdf\n800", "200\n300\n400", "400\nyyr\n900"]
    expected = 4800

-- ---------------------------------------------- --
--                 Property Tests                 --
-- ---------------------------------------------- --

test_propertyTests :: TestTree
test_propertyTests = testProperty "AOC Day 1: Property Based Tests" propertyTest

propertyTest :: Property
propertyTest =
  property $ do
    listOfIntLists <- forAll $ Gen.list (Range.linear 1 100) $ Gen.list (Range.linear 1 100) $ Gen.int (Range.linear 0 10000)
    let input = joinWithEmptyLine $ map (intercalate "\n" . map show) listOfIntLists
    let expected = maximum $ map sum listOfIntLists
    calculateMaxCalories input === fromIntegral expected
