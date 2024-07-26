module Solutions.Day3 where

import Day3.Solution (defaultInputFile, getSolutionElfGroups, getSolutionRucksacks)
import Helpers (fanThrough)
import Test.Hspec

spec_solution :: Spec
spec_solution = do
    describe "AOC Day 3: Unit Tests" $ do
        test1
        test2
        test3
        testWithOfficialInputFile

test1 :: Spec
test1 =
    do
        it "Should ensure that we can calculate the re-organization priority for each rucksack and elf group even with no arrangement configuration"
        $ (getSolutionRucksacks, getSolutionElfGroups) `fanThrough` input `shouldBe` expected
  where
    input = ""
    expected = (0, 0)

test2 :: Spec
test2 =
    do
        it "Should ensure that we can calculate the re-organization priority for each rucksack and skip invalid rucksacks"
        $ getSolutionRucksacks input `shouldBe` expected
  where
    input = "vJrwpWtwJgWrhcsFMMfFFhFp\noriw3243v\noriw3243\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nkjmlpc"
    expected = 54

test3 :: Spec
test3 =
    do
        it "Should ensure that we can calculate the re-organization priority for each elf group and skip invalid elf group configurations"
        $ getSolutionElfGroups input `shouldBe` expected
  where
    input = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\na344454lai\nllk\nBSBDzrSwrqccDDwbfcBjsRwggClslTRWGWGMFlsF\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw\naaZbbk\nllvkki\nmmioor"
    expected = 70

testWithOfficialInputFile :: Spec
testWithOfficialInputFile = do
    inp <- runIO (readFile defaultInputFile)
    let expectedAns = (2587, 8240)
    it "Ensures that functions output the right answer for the officially provided input file" $ (getSolutionElfGroups, getSolutionRucksacks) `fanThrough` inp `shouldBe` expectedAns
