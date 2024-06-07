module Solutions.Day1 where

import Day1.Solution (
    calcMaxCaloriesOverall,
    calcTopThreeCaloriesAsSum,
 )

import Test.Hspec

import Utils

spec_solution :: Spec
spec_solution = do
    describe "AOC Day 1: Unit Tests" $ do
        describe "Tests on valid calories inputs" $ do
            correctInputUnitTest1
            correctInputUnitTest2
            correctInputUnitTest3
            correctInputUnitTest4
        describe "Tests on invalid calories inputs" $ do
            invalidInputUnitTest1
            invalidInputUnitTest2
            invalidInputUnitTest3
            invalidInputUnitTest4
            invalidInputUnitTest5
            invalidInputUnitTest6

correctInputUnitTest1 :: Spec
correctInputUnitTest1 =
    it "Should ensure that we can discern the caloric total of the Elf that has the most calories" $ calcMaxCaloriesOverall input `shouldBe` expected
  where
    input = joinWithEmptyLine ["400\n500\n300", "200\n300", "700"]
    expected = 1200

correctInputUnitTest2 :: Spec
correctInputUnitTest2 =
    it "Should ensure that we can discern the caloric total of the three Elves that have the most calories" $ calcTopThreeCaloriesAsSum input `shouldBe` expected
  where
    input = joinWithEmptyLine ["700000", "800\n500\n1000", "20\n100\n665", "8000", "900\n70\n45", "800", "90"]
    expected = 700_000 + (800 + 500 + 1000) + 8000

correctInputUnitTest3 :: Spec
correctInputUnitTest3 =
    it "Should ensure that we can get the caloric total of all Elves if there are less than three" $ calcTopThreeCaloriesAsSum input `shouldBe` expected
  where
    input = joinWithEmptyLine ["700000", "800\n500\n1000"]
    expected = 700_000 + (800 + 500 + 1000)

correctInputUnitTest4 :: Spec
correctInputUnitTest4 =
    it "Should ensure that we can get the caloric total of all Elves if there are less than three (2)" $ calcTopThreeCaloriesAsSum input `shouldBe` expected
  where
    input = joinWithEmptyLine ["700000"]
    expected = 700_000

invalidInputUnitTest1 :: Spec
invalidInputUnitTest1 =
    it "Should ensure that calculateMaxCalories function outputs 0 on empty input" $ calcMaxCaloriesOverall input `shouldBe` expected
  where
    input = ""
    expected = 0

invalidInputUnitTest2 :: Spec
invalidInputUnitTest2 =
    it "Should ensure that calculateMaxCalories function outputs 0 if input contains only new lines" $ calcMaxCaloriesOverall input `shouldBe` expected
  where
    input = joinWithEmptyLine ["\n", "\n", "\n"]
    expected = 0

invalidInputUnitTest3 :: Spec
invalidInputUnitTest3 =
    it "Should ensure that calculateMaxCalories function skips non-integers in calculation" $ calcMaxCaloriesOverall input `shouldBe` expected
  where
    input = joinWithEmptyLine ["aString\n4000\nfdf\n800", "200\n300\n400", "400\nyyr\n900"]
    expected = 4800

invalidInputUnitTest4 :: Spec
invalidInputUnitTest4 =
    it "Should ensure that calcTopThreeCaloriesAsSum function outputs 0 on empty input" $ calcTopThreeCaloriesAsSum input `shouldBe` expected
  where
    input = ""
    expected = 0

invalidInputUnitTest5 :: Spec
invalidInputUnitTest5 =
    it "Should ensure that calcTopThreeCaloriesAsSum function outputs 0 if input contains only new lines" $ calcTopThreeCaloriesAsSum input `shouldBe` expected
  where
    input = joinWithEmptyLine ["\n", "\n", "\n"]
    expected = 0

invalidInputUnitTest6 :: Spec
invalidInputUnitTest6 =
    it "Should ensure that calcTopThreeCaloriesAsSum function skips non-integers in calculation" $ calcTopThreeCaloriesAsSum input `shouldBe` expected
  where
    input = joinWithEmptyLine ["aString\n4000\nfdf\n800", "200\n300\n400", "400\nyyr\n900", "9000\nerf\n1000", "5000\nerfr", "efr\n800"]
    expected = 4800 + 10_000 + 5000
