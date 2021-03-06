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
      parse (char 'c') "c" @?= 
        Right ('c', ParserState 
          {position = (1,2), remainder = ""}),
    testCase "Invalid char yields error" $ 
      (parse (char 'c') "b") @?= 
        Left (UnsatisfiedPredicate "char c", ParserState
          {position = (1,1), remainder = "b"})
  ]

-- |Tests for parser sat (parses character only if is satisfies predicate)
testSat = [
  testCase "Consuming one digit" $ 
    parse (sat isDigit) "123" @?= Right ('1',ParserState {position = (1,2), 
                                                          remainder = "23"}),
  testCase "Letter is not a digit" $ 
    parse (sat isDigit) "a23" @?= 
      Left (UnsatisfiedPredicate "general", ParserState {position = (1,1),
                                             remainder = "a23"})
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
                                                    remainder = " ba"}),
  testCase "Fails on empty input" $
    parse word "" @?= Left (EmptyRemainder "item",ParserState {position = (1,1), remainder = ""})
  ]

-- |Tests for parser token (skip space-symbols and parse with parser p)
tokenTestGroup = testGroup "Test cases for parser token" $ testToken 

testToken = [
  testCase "If prefix spaces isn't presented -- just applies the parser" $
    parse (token (string "hibro!")) "hibro!" @?= 
      Right ("hibro!",ParserState {position = (1,7), remainder = ""}), 
  testCase "Skips prefix spaces and parses string" $ 
    parse (token (string "hibro!")) " \n  \t hibro!" @?= 
      Right ("hibro!",ParserState {position = (2,15), remainder = ""}),
  testCase "If internal parser fails -- so fails token" $
    parse (token (string "hibro!")) "Hi, Bro!" @?= 
      Left (UnsatisfiedPredicate "string hibro!", 
            ParserState{position = (1,1), remainder = "Hi, Bro!"}),
  testCase "Fails on empty input" $
    parse (token (string "hibro!")) "" @?= 
      Left (EmptyRemainder "item",ParserState {position = (1,1), remainder = ""})
  ]

-- |Tests for parser bracket (parses a thing enclosed by brackets)
bracketTestGroup = testGroup "Test cases for parser bracket" $ testBracket 

testBracket = [
  testCase "Parses string in simple char brackets" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1 + 1)" @?=
      Right ("1 + 1",ParserState {position = (1,8), remainder = ""}),
  testCase "Fails if first bracket parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "[1 + 1)" @?= 
      Left (BracketError "invalid opening bracket",
            ParserState {position = (1,1), remainder = "[1 + 1)"}),
  testCase "Fails if second bracket parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1 + 1]" @?= 
      Left (BracketError "invalid closing bracket",
            ParserState {position = (1,7), remainder = "]"}),
  testCase "Fails if internal parser does" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "(1+1)"   @?= 
      Left (UnsatisfiedPredicate "string 1 + 1",
            ParserState {position = (1,2), remainder = "1+1)"}),
  testCase "Fails on empty input" $ 
    parse (bracket (char '(') (string "1 + 1") (char ')')) "" @?= 
      Left (EmptyRemainder "item",ParserState {position = (1,1), remainder = ""})
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
