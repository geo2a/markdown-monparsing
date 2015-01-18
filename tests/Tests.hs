module Main (
    main
) where

import Test.Tasty
--import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Char

import Parsers

-- TODO: Разобраться с QuickCheck и добавить тесты 
-- TODO: Возможно, стоит продублировать некоторые тесты с Data.Text

-----------------------------------------------------------------
----------------Test for single character parsers----------------
-----------------------------------------------------------------

singlesTests = testGroup "Tests for single character parsers" $ 
  concat [testChar, testSat]

-- |Tests for parser char (parses specified character)
testChar = 
  [
    testCase "Consuming one specified char" $ 
      (parse (char 'c') "c") @?= [('c',"")],
    testCase "No char -- empty list" $ (parse (char 'c') "b") @?= []
  ]

-- |Tests for parser chat (parses character only if is satisfies predicate)
testSat = [
  testCase "Consuming one digit" $ parse (sat isDigit) "123" @?= [('1',"23")],
  testCase "Letter is not a digit" $ parse (sat isDigit) "a23" @?= []
  ]

-----------------------------------------------------------------
--------------Test for parsers for groups of chars---------------
-----------------------------------------------------------------

manysTests = testGroup "Tests for multy character parsers" $ 
  [wordTestGroup, tokenTestGroup, bracketTestGroup]

-- |Tests for parser word (parses non-empty string of letters)
wordTestGroup = testGroup "Test cases for parser word" $ testWord 

testWord = [
  testCase "Empty word parsed as empty" $ parse word "" @?= [],
  testCase "Parse until meeting with a space" $ 
    parse word "ab ba" @?=  [("ab"," ba")],
  testCase "Fails on empty input" $
    parse word "" @?= [] 
  ]

-- |Tests for parser token (skip space-symbols and parse with parser p)
tokenTestGroup = testGroup "Test cases for parser token" $ testToken 

testToken = [
  testCase "If prefix spaces isn't presented -- just applies the parser" $
    parse (token (string "hibro!")) "hibro!" @?= [("hibro!","")], 
  testCase "Skips prefix spaces and parses string" $ 
    parse (token (string "hibro!")) " \n  \t hibro!" @?= [("hibro!","")],
  testCase "If internal parser fails -- so fails token" $
    parse (token (string "hibro!")) "Hi, Bro!" @?= [],
  testCase "Fails on empty input" $
    parse (token (string "hibro!")) "" @?= []  
  ]

-- |Tests for parser bracket (parses a thing enclosed by brackets)
bracketTestGroup = testGroup "Test cases for parser bracket" $ testBracket 

testBracket = [
  testCase "Parses string in simple char brackets" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1 + 1)" @?=
      [("1 + 1","")],
  testCase "Fails if first bracket parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "[1 + 1)" @?= [],
  testCase "Fails if second bracket parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1 + 1]" @?= [],
  testCase "Fails if internal parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1+1)"   @?= [],
  testCase "Fails on empty input" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "" @?= []
  ]


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
