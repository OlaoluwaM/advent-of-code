module Solutions.Day4 where

import Day4.Solution (computeSolutionOne, computeSolutionTwo, defaultInputFile)
import Helpers (fanThrough)
import Test.Hspec

import Data.Text.IO qualified as TIO

spec_solution :: Spec
spec_solution = do
    describe "AOC Day 4: Unit Tests" $ do
        test1
        test2
        test3
        testWithOfficialInputFile

test1 :: Spec
test1 =
    it "Should ensure that function for solution one works even if input contains invalid assignment pairings" $
        let inp = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8\nfrefefer\n888-0,93-10,34958\n1-10,5-8\n15-18,11-20\n1-10,9-12,12-20"
            expectedAns = 4
         in computeSolutionOne inp `shouldBe` expectedAns

test2 :: Spec
test2 =
    it "Should ensure that function for solution two works even if input contains invalid assignment pairings" $
        let inp = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8\nfrefefer\n888-0,93-10\n1-6,5-8\n1-10,5-8\n1-10,9-12,12-20"
            expectedAns = 6
         in computeSolutionTwo inp `shouldBe` expectedAns

test3 :: Spec
test3 =
    it "Should ensure that solution functions return 0 if input is empty" $
        let input = "" in (computeSolutionOne, computeSolutionTwo) `fanThrough` input `shouldBe` (0, 0)

testWithOfficialInputFile :: Spec
testWithOfficialInputFile = do
    inp <- runIO (TIO.readFile defaultInputFile)
    let expectedAns = (456, 808)
    it "Ensures that functions output the right answer for the officially provided input file" $ (computeSolutionOne, computeSolutionTwo) `fanThrough` inp `shouldBe` expectedAns
