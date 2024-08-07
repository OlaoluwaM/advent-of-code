module Solutions.Day2 where

import Data.Text.IO qualified as T
import Day2.Solution (defaultInputFile, runRPSPart1, runRPSPart2)
import Helpers (fanThrough)
import Test.Hspec

-- {-
--   Domain Rules

--   A -> Rock
--   B -> Paper
--   C -> Scissors

--   Rock -> 1pt (X)
--   Paper -> 2pt (Y)
--   Scissors -> 3pt (Z)

--   Win -> 6pt (Z)
--   Lose -> 0pt (X)
--   Draw -> 3pt (Y)
-- -}

spec_solution :: Spec
spec_solution = do
    describe "AOC Day 2: Unit Tests" $ do
        test1
        test2
        testWithOfficialInputFile

test1 :: Spec
test1 =
    do
        it "Should ensure that we can calculate the total number of points earned after a set of RPS rounds (for all parts, even with invalid inputs)"
        $ (runRPSPart1, runRPSPart2) `fanThrough` input `shouldBe` expected
  where
    input = "A Z\nB Y\nC X\nL P\n9 L\nB X\nK P\n8 O\nH \n"
    expected = (16, 16)

test2 :: Spec
test2 = do
    it "Should ensure that we output 0 on empty input" $ (runRPSPart1, runRPSPart2) `fanThrough` input `shouldBe` expected
  where
    input = ""
    expected = (0, 0)

testWithOfficialInputFile :: Spec
testWithOfficialInputFile = do
    input <- runIO (T.readFile defaultInputFile)
    let expectedAns = (13726, 12855)
    it "Ensures that functions output the right answer for the officially provided input file" $ (runRPSPart2, runRPSPart1) `fanThrough` input `shouldBe` expectedAns
