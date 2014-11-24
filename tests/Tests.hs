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

--testWord = [
--  ([],parse word []),
--  ([("ab"," ba"),("a","b ba")],parse word "ab ba")
--  ]

-----------------------------------------------------------------
--------------Test for parsers for groups of chars---------------
-----------------------------------------------------------------

allTest = testChar ++ testSat

main = runTestTT . TestList . map (uncurry $ testAssertEqual "") $ allTest
  where 
    testAssertEqual s a b = TestCase $ assertEqual s a b 

