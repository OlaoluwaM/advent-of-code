module Solutions.Day4 where

-- import Data.List (intercalate)
-- import Day4.Solution (solution)
-- import Test.Hspec

-- spec_solution :: Spec
-- spec_solution = do
--     describe "AOC Day 4: Unit Tests" $ do
--         describe "Tests on valid inputs" $ do
--             correctInputUnitTest1
--             correctInputUnitTest2
--             correctInputUnitTest3
--         describe "Tests on invalid inputs" $ do
--             incorrectInputUnitTest1
--             incorrectInputUnitTest2
--             incorrectInputUnitTest3

-- correctInputUnitTest1 :: SpecWith ()
-- correctInputUnitTest1 = do
--     it "Should ensure that we can calculate the total number of range pairs with complete overlap" $ solution input `shouldBe` expected
--   where
--     input = mkInput [["90-100", "90-95"], ["1-10", "10-20"], ["30-50", "31-70"], ["20-30", "40-50"], ["40-80", "50-100"]]
--     expected = 1

-- correctInputUnitTest2 :: SpecWith ()
-- correctInputUnitTest2 = do
--     it "Should ensure that we can calculate the total number of range pairs with complete overlap regardless of some faulty inputs" $ solution input `shouldBe` expected
--   where
--     input = mkInput [["9-90"], [], ["1-10", "5-5"], ["9-9", "10-10"], ["3-3"]]
--     expected = 1

-- correctInputUnitTest3 :: SpecWith ()
-- correctInputUnitTest3 = do
--     it "Should ensure that we can calculate the total number of range pairs if all range pairs overlap" $ solution input `shouldBe` expected
--   where
--     rawInput = [["9-90", "14-50"], ["1-20", "4-10"], ["1-10", "5-5"], ["9-19", "10-10"], ["3-50", "8-22"], ["2-2000000", "40-55"]]
--     input = mkInput rawInput
--     expected = length rawInput

-- incorrectInputUnitTest1 :: SpecWith ()
-- incorrectInputUnitTest1 = do
--     it "Should ensure that we can handle a case where no range pairs are supplied" $ solution input `shouldBe` expected
--   where
--     input = mkInput []
--     expected = 0

-- incorrectInputUnitTest2 :: SpecWith ()
-- incorrectInputUnitTest2 = do
--     it "Should ensure that we can handle a case where invalid ranges have been supplied" $ solution input `shouldBe` expected
--   where
--     input = mkInput [["a-b", "a"], ["c-d", "d"], ["9-99", "40-44"], ["2", "2"], ["22"]]
--     expected = 3

-- incorrectInputUnitTest3 :: SpecWith ()
-- incorrectInputUnitTest3 = do
--     it "Should ensure that we can handle a case where invalid ranges (negatives) have been supplied" $ solution input `shouldBe` expected
--   where
--     input = mkInput [["-1-10", "-5-10"], ["-1-20", "2-10"], ["-5"], ["-4-8", "10-20"]]
--     expected = 0

-- -- ---------------------------------------------- --
-- --                     Helpers                    --
-- -- ---------------------------------------------- --
-- mkInput :: [[String]] -> String
-- mkInput = intercalate "\n" . map (intercalate ",")
