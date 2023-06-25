module Solutions.DayTwoTest where

import Data.Either (isLeft)
import DayTwo.SolutionAlt (runRPS)
import Test.Hspec

spec_solution :: Spec
spec_solution = do
  describe "AOC Day 2: Unit Tests" $ do
    describe "Tests on valid inputs" $ do
      correctInputUnitTest1
      correctInputUnitTest2
      correctInputUnitTest3
    describe "Tests on invalid inputs" $ do
      incorrectInputUnitTest1
      incorrectInputUnitTest2
      incorrectInputUnitTest3

correctInputUnitTest1 :: SpecWith ()
correctInputUnitTest1 = do
  it "Should ensure that we can calculate the total number of points earned after a set of RPS rounds" $ runRPS input `shouldBe` expected
  where
    input = "A Z\nB Y\nC X\nB Y\n"
    expected = mkExpectedOutput [3, 5, 7, 5]

correctInputUnitTest2 :: SpecWith ()
correctInputUnitTest2 = do
  it "Should ensure that we can calculate the total number of points earned after a set of RPS rounds 2" $ runRPS input `shouldBe` expected
  where
    input = "A Z\nC X\nB Y\nA Z\nC Y\nA Y\nC Z\n"
    expected = mkExpectedOutput [3, 7, 5, 3, 2, 8, 6]

correctInputUnitTest3 :: SpecWith ()
correctInputUnitTest3 = do
  it "Should ensure that we output 0 on empty input" $ runRPS input `shouldBe` expected
  where
    input = ""
    expected = Right 0

incorrectInputUnitTest1 :: SpecWith ()
incorrectInputUnitTest1 = do
  it "Should ensure that we report an error if a round includes illegal play options for either the opponent or player" $ runRPS input `shouldSatisfy` isLeft
  where
    input = "A L\nL I\nO P\nU D\nA Z\nH L\n"

incorrectInputUnitTest2 :: SpecWith ()
incorrectInputUnitTest2 = do
  it "Should ensure that we report an error if a round includes illegal play options for either the opponent or player 2" $ runRPS input `shouldSatisfy` isLeft
  where
    input = "A L\n11 90\nO p\nA A\ne e\na 2\n"

incorrectInputUnitTest3 :: SpecWith ()
incorrectInputUnitTest3 = do
  it "Should ensure that we report an error if even a single round includes an illegal play option for either the opponent or player" $ runRPS input `shouldSatisfy` isLeft
  where
    input = "A Z\nC X\ny 2\nA Z\nC Y\nA Y\nC Z\n"

-- ---------------------------------------------- --
--                     Helpers                    --
-- ---------------------------------------------- --
mkExpectedOutput :: [Integer] -> Either String Integer
mkExpectedOutput roundResults = return $ sum roundResults
