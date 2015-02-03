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
      (parse (char 'c') "c") @?= Right ('c',ParserState {position = (1,2), 
                                                         input = ""}),
    testCase "No char -- error" $ 
      (parse (char 'c') "b") @?= Left ""
  ]

-- |Tests for parser chat (parses character only if is satisfies predicate)
testSat = [
  testCase "Consuming one digit" $ 
    parse (sat isDigit) "123" @?= Right ('1',ParserState {position = (1,2), 
                                                          input = "23"}),
  testCase "Letter is not a digit" $ 
    parse (sat isDigit) "a23" @?= Left ""
  ]

-------------------------------------------------------------------
----------------Test for parsers for groups of chars---------------
-------------------------------------------------------------------

manysTests = testGroup "Tests for multy character parsers" $ 
  [wordTestGroup, tokenTestGroup, bracketTestGroup]

-- |Tests for parser word (parses non-empty string of letters)
wordTestGroup = testGroup "Test cases for parser word" $ testWord 

testWord = [
  testCase "Parse until meeting with a space" $ 
    parse word "ab ba" @?= Right ("ab",ParserState {position = (1,3), 
                                                    input = " ba"}),
  testCase "Fails on empty input" $
    parse word "" @?= Left ""
  ]

-- |Tests for parser token (skip space-symbols and parse with parser p)
tokenTestGroup = testGroup "Test cases for parser token" $ testToken 

testToken = [
  testCase "If prefix spaces isn't presented -- just applies the parser" $
    parse (token (string "hibro!")) "hibro!" @?= 
      Right ("hibro!",ParserState {position = (1,7), input = ""}), 
  testCase "Skips prefix spaces and parses string" $ 
    parse (token (string "hibro!")) " \n  \t hibro!" @?= 
      Right ("hibro!",ParserState {position = (2,15), input = ""}),
  testCase "If internal parser fails -- so fails token" $
    parse (token (string "hibro!")) "Hi, Bro!" @?= Left "",
  testCase "Fails on empty input" $
    parse (token (string "hibro!")) "" @?= Left ""  
  ]

-- |Tests for parser bracket (parses a thing enclosed by brackets)
bracketTestGroup = testGroup "Test cases for parser bracket" $ testBracket 

testBracket = [
  testCase "Parses string in simple char brackets" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1 + 1)" @?=
      Right ("1 + 1",ParserState {position = (1,8), input = ""}),
  testCase "Fails if first bracket parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "[1 + 1)" @?= 
      Left "",
  testCase "Fails if second bracket parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1 + 1]" @?= 
      Left "",
  testCase "Fails if internal parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1+1)"   @?= 
      Left "",
  testCase "Fails on empty input" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "" @?= 
      Left ""
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
