module Main (
    main
) where

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Char

import Parsers

-----------------------------------------------------------------
----------------Test for single character parsers----------------
-----------------------------------------------------------------

-- |Tests for parser char (parses specified character)
testChar = 
  [
    testCase "Consuming one specified char" $ (parse (char 'c') "c") @?= [('c',"")],
    testCase "No char -- empty list" $ (parse (char 'c') "b") @?= []
  ]

testSat = [
  testCase "Consuming one digit" $ parse (sat isDigit) "123" @?= [('1',"23")],
  testCase "Letter is not a digit" $ parse (sat isDigit) "a23" @?= []
  ]

singlesTests = testGroup "Tests for single character parsers" $ 
  concat [testChar, testSat]

-----------------------------------------------------------------
--------------Test for parsers for groups of chars---------------
-----------------------------------------------------------------

testWord = [
  testCase "Empty word parsed as empty" $ parse word [] @?= [],
  testCase "Parse until meeting with a space" $ 
    parse word "ab ba" @?=  [("ab"," ba")]
  ]

manysTests = testGroup "Tests for multy character parsers" $ 
  concat [testWord]

--manysTests = buildTestList $ concat [testWord]
-----------------------------------------------------------------
-------------------------Running tests---------------------------
-----------------------------------------------------------------

--buildTestList :: (Eq a, Show a) => [(a,a)] -> Test
--buildTestList = testList . map (uncurry $ testAssertEqual "")

unitTests = testGroup "Unit Tests" [singlesTests,manysTests]

tests :: TestTree
tests = testGroup "Tests" [unitTests]

--main = runTestTT `mapM` [singlesTests, manysTests]
     
main = defaultMain tests
