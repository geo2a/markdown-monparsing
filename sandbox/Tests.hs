module Main (
    main
) where

import Test.HUnit
import Data.Char

import Parsers

-----------------------------------------------------------------
----------------Test for single character parsers----------------
-----------------------------------------------------------------

-- |Tests for parser char (parses specified character)
testChar = [
  ([('c',"")],parse (char 'c') "c"), 
  ([],parse (char 'c') "b") 
  ]

testSat = [
  ([('1',"23")],parse (sat isDigit) "123"),
  ([],parse (sat isDigit) "a23")
  ]

singlesTests = buildTestList $ concat [testChar, testSat]

-----------------------------------------------------------------
--------------Test for parsers for groups of chars---------------
-----------------------------------------------------------------

testWord = [
  ([],parse word []),
  ([("ab"," ba"),("a","b ba")],parse word "ab ba")
  ]

manysTests = buildTestList $ concat [testWord]
-----------------------------------------------------------------
-------------------------Running tests---------------------------
-----------------------------------------------------------------

testAssertEqual :: (Eq a, Show a) => String -> a -> a -> Test
testAssertEqual s a b = TestCase $ assertEqual s a b

buildTestList :: (Eq a, Show a) => [(a,a)] -> Test
buildTestList = TestList . map (uncurry $ testAssertEqual "")

main = runTestTT `mapM` [singlesTests, manysTests]
     

