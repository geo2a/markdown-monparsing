module Parsers where

import Prelude hiding (splitAt)
import Control.Applicative
import Control.Monad
import Data.Maybe (maybeToList)
import Data.Monoid (mempty, mappend, mconcat)
import qualified Data.Monoid.Textual as TM
import Data.Monoid.Factorial (splitAt)
import Data.Text (pack) -- для теста совместимости

--import Helpers

newtype Parser t a = Parser (t -> [(a,t)])

parse :: TM.TextualMonoid t => Parser t p -> t -> [(p,t)]
parse (Parser p) = p

instance TM.TextualMonoid t => Functor (Parser t) where
  fmap = liftM

instance TM.TextualMonoid t => Applicative (Parser t) where
  pure = return
  (<*>) = ap

instance TM.TextualMonoid t => Alternative (Parser t) where
  (<|>) = mplus
  empty = mzero   

instance TM.TextualMonoid t => Monad (Parser t) where
  return a = Parser (\cs -> [(a,cs)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs' |
                            (a,cs') <- parse p cs])
  fail _   = Parser (\cs -> [])

instance TM.TextualMonoid t => MonadPlus (Parser t) where
  mzero = Parser (\cs -> [])
  p `mplus` q = Parser (\cs -> 
    let ps = parse p cs in 
      if null ps then parse q cs else ps)

-- |Consumes one symbol of any kind 
item :: TM.TextualMonoid t => Parser t Char
item =  Parser (maybeToList . TM.splitCharacterPrefix)

-- |Consumes item only if it satisfies predicate
-- В очередной раз восхитился супер-абстрактности монадического кода!
-- когда обобщал эту функцию до TextualMonoid, переписал только сигнатуру! 
sat :: TM.TextualMonoid t => (Char -> Bool) -> Parser t Char
sat p = do
  x <- item 
  guard $ p x
  return x

------------------Парсеры для одиночных символов----------------

-- |Consumes item only if it is equal to specified char
char :: TM.TextualMonoid t => Char -> Parser t Char
char x = sat (\y -> x == y)

---- |Decimal digit
--digit :: Parser Char
--digit = sat isDigit

---- |Lowercase letter
--lower :: Parser Char
--lower = sat isLower

---- |Uppercase letter
--upper :: Parser Char
--upper = sat isUpper

---- |Anycase letter
--letter :: Parser Char
--letter = lower <|> upper

---- |Anycase letter or decimal digit
--alphanum :: Parser Char
--alphanum = letter <|> digit

--newline :: Parser Char
--newline  = char '\n'  
------------------Парсеры для групп символов----------------

---- ОФИГЕТЬ!! Добавив инстанс для Alternative, 
---- мы даром получаем many == Alternative.many и 
---- many1 == Alternative.some 

---- |Parse a specified string
--string :: String -> Parser String
--string = mapM char

---- |Word (non-empty string of letters)
--word :: Parser String 
--word = some letter 

---- |Parse a token with specific parser, throw away any trailing spaces
--token :: Parser a -> Parser a
--token p = spaces >> p

---- |Parse a symbolic token, just a specification of token parser
--symbol :: String -> Parser String
--symbol cs = token (string cs)

---- TODO: Это, похоже, не востребовано
---- |Parse identifier, i.e. word with leading lowercase letters and trailing alphanums 
--ident :: Parser String
--ident = do
--  x <- lower
--  xs <- many alphanum
--  return (x:xs)

---- TODO: Это, похоже, не востребовано
---- |Same as ident but fails if argument is a member of list of keywords
--identifier :: [String] -> Parser String
--identifier kwords = do
--  x <- ident
--  if elem x kwords then return "" else return x

---- |Parse a thing enclosed by brackets
--bracket :: Parser a -> Parser b -> Parser c -> Parser b
--bracket open p close = do 
--  open
--  x <- p
--  close
--  return x

------------------"Lexical issues"----------------
--spaces :: Parser String
--spaces = many (sat isSpace)

------------------Repetitions with seporators----------------
---- TODO: parse (word `sepby1` spaces) "aaa@aaa" == [(["aaa"],"@aaa")]
---- Разве не должен парсер sepby1 фейлится в таком случае? Разобраться.
---- Может быть, стоит добавить что-то вроде: guard $ null as?   

--sepby :: Parser a -> Parser b -> Parser [a]
--p `sepby` sep = (p `sepby1` sep) <|> return []

--sepby1 :: Parser a -> Parser b -> Parser [a]
--p `sepby1` sep = do 
--  a <- p
--  as <- many (sep >> p)
--  return (a:as)
--  -- or this: (:) <$> p <∗> many (sep >> p)
