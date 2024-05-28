module Solutions.Day3Test where

import Day3.Solution (part1Solution)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.Hedgehog
import Utils (joinWithNewLine)

test_propertyTests :: TestTree
test_propertyTests =
  testGroup
    "AOC Day 3: Property Based Tests"
    [ testProperty "With valid inputs, random even length strings" propertyTestWithValidInputs
    , testProperty "With some invalid inputs, random length strings" propertyTestWithInvalidInputs2
    , expectFail $ testProperty "With some invalid inputs, random alpha-numeric even length strings" propertyTestWithInvalidInputs1
    ]

propertyTestWithValidInputs :: Property
propertyTestWithValidInputs =
  property $ do
    len <- forAll $ Gen.integral $ Range.linear 1 100
    listOfEvenLenStr <- forAll $ Gen.list (Range.linear 1 100) $ Gen.string (Range.singleton $ 2 * len) Gen.alpha
    let input = joinWithNewLine listOfEvenLenStr
    evalEither $ part1Solution input >> Right ()

propertyTestWithInvalidInputs1 :: Property
propertyTestWithInvalidInputs1 =
  property $ do
    len <- forAll $ Gen.integral $ Range.linear 1 100
    listOfStr <- forAll $ Gen.list (Range.linear 1 100) $ Gen.string (Range.singleton $ 2 * len) Gen.alphaNum
    let input = joinWithNewLine listOfStr
    evalEither $ part1Solution input >> Right ()

propertyTestWithInvalidInputs2 :: Property
propertyTestWithInvalidInputs2 =
  property $ do
    listOfStr <- forAll $ Gen.list (Range.linear 1 100) $ Gen.string (Range.linear 1 100) Gen.alpha
    let input = joinWithNewLine listOfStr
    evalEither $ part1Solution input >> Right ()
